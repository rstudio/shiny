# Shiny × MCP Apps — architecture

How {shiny}'s experimental MCP Apps (SEP-1865) support is built, as
implemented on branch `schloerke/shiny-mcp-app-protocol` (PR
rstudio/shiny#4407). This is the "how it all fits together" document;
`design-spec.md` records only the original Phase 1 design, and
`research-notes.md` the pre-implementation research.

## The problem

MCP Apps hosts (Claude Desktop, claude.ai, basic-host, ...) render a tool's
UI as **static HTML in a sandboxed iframe**. The iframe may not load
external resources, and its only guaranteed channel to the outside world is
**postMessage JSON-RPC to the host**, which the host relays to the MCP
server as `tools/call` requests. There is **no server→app push**.

Shiny's model is the opposite: a live server process, a WebSocket for
reactivity, and HTTP side channels (uploads, downloads, server-side
selectize/DataTables, dynamic dependencies) against the app's own origin.

Everything below is about bridging those two models without changing app
code.

## Big picture

```
options(shiny.mcp = TRUE)
┌─────────────────────────── R process ────────────────────────────┐
│  httpuv (the app's normal port)                                  │
│  ├── /            normal Shiny app (browser use unchanged)       │
│  ├── /websocket/  normal Shiny websocket                         │
│  └── /mcp         MCP JSON-RPC endpoint (streamable HTTP, JSON)  │
│                    └── mcpDispatch() ── tools / resources        │
│  optional: stdio transport (newline JSON-RPC) → same dispatch    │
└──────────────────────────────────────────────────────────────────┘
          ▲ tools/call                     ▲ real WebSocket (fast path)
┌─────────┴───────────┐          ┌─────────┴──────────┐
│  MCP host (Claude)  │ postMsg  │  sandboxed iframe   │
│  conversation UI    │ ◄──────► │  single-file HTML   │
└─────────────────────┘          │  app + JS bridge    │
                                 └────────────────────┘
```

Three transports, one dispatcher:

1. **Streamable HTTP** — `POST /mcp` on the app's own httpuv port
   (`R/mcp-server.R:mcpHttpHandler`). JSON responses only (no SSE);
   `GET /mcp` returns 405. CORS headers are sent on the `/mcp` endpoint
   only.
2. **stdio** — `options(shiny.mcp.stdio = TRUE)` additionally speaks
   newline-delimited JSON-RPC on stdin/stdout (`R/mcp-stdio.R`), so local
   hosts can launch the R process directly (`claude mcp add -- Rscript
   ...`). Non-blocking `file("stdin")` polled from the `later` loop
   coexists with httpuv. stdout is reserved for the protocol; Windows
   unsupported.
3. **postMessage tunnel / direct WebSocket** — how the *iframe* reaches
   the server; see "Two transports for reactivity" below.

All three go through `mcpDispatch(body, uiHandler, wsHandler)`
(`R/mcp-server.R`), which implements `initialize`, `ping`, `tools/list`,
`tools/call`, `resources/list`, `resources/read`. The stdio loop reuses
the closure `createAppHandlers()` stashes in `.globals$mcpDispatch`.

## What the host sees

- **Tools** (`mcpToolsList()`):
  - the *app tool* (default `open_shiny_app`; customizable via
    `options(shiny.mcp.tool = list(name=, description=, inputSchema=))`)
    with `_meta.ui.resourceUri` pointing at the app resource — calling it
    makes the host render the iframe;
  - five *internal tunnel tools* `_shiny_connect/_send/_receive/_close/
    _http`, marked `_meta.ui.visibility = ["app"]` so only the iframe (not
    the model) can call them;
  - *author tools* registered via `registerMcpTool(ellmer::tool(...))` —
    plain R functions exposed to the model, run sessionless in the server
    process (character → text content, list → structuredContent, promises
    supported, errors → `isError`). The ellmer→JSON-Schema conversion
    mirrors `mcptools::tool_as_json()`. `registerMcpTool()` validates
    eagerly — a reserved/colliding name or a non-`ellmer::tool()` argument
    causes a hard `stop()` at registration time.
- **One resource**: `ui://shiny/app` (or `ui://shiny/<appId>`, see
  gateway section), mime `text/html;profile=mcp-app`.

Serialization gotcha: partially named R lists serialize as JSON *objects*;
`tools/list` needs `unname()` to stay a JSON array. R-side parse tests
don't catch this — only raw-JSON/curl checks do.

## The single-file resource (R/mcp-app.R)

`renderMcpAppHtml(uiHandler, config)` renders the app's UI page and makes
it self-contained, because the sandbox can't fetch anything:

- `inlineHtmlAssets()` inlines every `<script src>` and `<link
  rel=stylesheet>` (resolved through resourcePaths + shared assets).
- `rewriteCssUrls()` converts relative `url()` refs inside inlined CSS
  (webfonts, images) to `data:` URIs. Skips `data:`/`blob:`/absolute/`#`
  refs and legacy `.ttf/.otf/.eot` fallbacks (browsers never fetch them
  once the inlined woff2 loads; keeps the resource ~2.3 MB vs ~5 MB).
  Regex needs `perl = TRUE` (TRE fails backreferences to empty-matching
  groups). `jsonlite::base64_enc()` line-wraps — strip `[\r\n]` or a raw
  newline inside a quoted CSS string silently truncates the sheet.
- The bundled bridge `inst/www/shared/shiny-mcp-bridge.js` (built from
  `srcts/src/mcp/`, bundles `@modelcontextprotocol/ext-apps`) plus a
  `window.__shinyMcpConfig__ = {appId?, directBase?, displayModes}` blob
  are injected before `</body>`.
- Plots and other `session$fileUrl()` outputs already arrive as `data:`
  URIs over the Shiny protocol, so they work in the sandbox unchanged.
- **Dynamic** htmlDependencies (insertUI, renderUI) would normally be
  fetched over HTTP; for MCP sessions `processDeps()` (`R/html-deps.R`)
  swaps in `mcpInlineDependency()`, which inlines them into the message.

## Two transports for reactivity

The bridge (`srcts/src/mcp/index.ts`) overrides `Shiny.createSocket()` —
the seam Shiny already exposes — with `McpHybridWebSocket`
(`hybridSocket.ts`):

1. **Direct fast path**: if the resource declared the app's origin in
   `_meta.ui.csp.connectDomains` and the host honors it, a real WebSocket
   to `<directBase>/websocket/?mcp=1` connects in ~one RTT (2.5 s
   timeout). Native latency; used by basic-host. The `?mcp=1` marker makes
   `isMcpSession()` true for these sessions.
2. **Tunnel fallback** (`tunnelSocket.ts`): frames go out via
   `tools/call _shiny_send` and come back by long-polling
   `_shiny_receive` (15 s), emulating push. Server side
   (`R/mcp-tunnel.R`) a duck-typed `McpConnection` (same `$send/$close/
   $onMessage/...` surface as a real websocket) is fed to the normal
   `createAppHandlers()$ws()` handler — Shiny doesn't know the
   difference. Its request env carries `HTTP_MCP_TUNNEL = "1"` (the other
   `isMcpSession()` marker). Idle connections are GC'd after 60 s by a
   sweeper.

HTTP side channels are handled by `tunnelXhr.ts`: `window.XMLHttpRequest`
is replaced by a class that routes same-origin/relative requests through
`_shiny_http` (method/path/headers/base64 body → full Rook request against
`mcpTunnelHttpChain()` = session handler + shiny www + resourcePaths) and
delegates foreign URLs to the native XHR. Download links are intercepted
and served via blob URLs. **Side channels stay tunneled even in direct
mode** — adding permissive CORS to localhost Shiny apps would let any
website read app responses, so `_shiny_http` instead accepts a missing
`connectionId` (deliberate security decision; see QUESTIONS.md §3).

## Session API (R/mcp-session.R, exported)

`isMcpSession()`, `mcpToolInput()` (arguments the model passed, reactive
via `session$clientData$mcp_tool_input`), `mcpHostContext()` (theme /
locale / displayMode), `mcpUpdateModelContext(text=, data=)`,
`mcpSendMessage(text)`, `mcpRequestDisplayMode(mode)`. The outbound ones
ride `session$sendCustomMessage("shiny.mcp.*")`; the bridge maps them to
`app.updateModelContext()` / `sendMessage()` / `requestDisplayMode()` and
no-ops (returning `FALSE`) outside MCP. Host→app events (tool input/result,
host context changes, theme/style variables) flow the other way into
`session$clientData` and CSS variables via the ext-apps SDK style helpers.

## Deployment: finding the app's external URL

For the direct fast path the server must tell the sandbox where the app
*externally* lives — behind proxies the request's `Host` is an internal
`127.0.0.1:<port>`. `mcpDirectBase()` (R/mcp-server.R) resolves, in order:

1. `options(shiny.mcp.origin=)` — explicit override, may include a path;
2. `RStudio-Connect-App-Base-Url` — what Posit Connect actually sends to
   Shiny content (verified on connect.posit.it; Connect does **not** send
   `X-RSC-Request` to Shiny content, and rsconnect never bundles its
   `rsconnect/*.dcf` records, so this header is the working signal there);
3. `X-RSC-Request` — kept for Connect API content; strip trailing `/mcp`;
4. `X-Redx-Frontend-Name` — shinyapps.io's schemeless `host/path`
   (verified on barret.shinyapps.io; that platform also sends
   `RStudio-Connect-App-Base-Url` *empty*, hence the nzchar guards) +
   `X-Forwarded-Proto`;
5. a deployment record next to the app whose URL host matches the serving
   host (rsconnect `rsconnect/**/*.dcf` `url`, Posit Publisher
   `.posit/publish/deployments/*.toml` `direct_url`) — host-matching
   prevents a local run of a deployed app dir from pointing the iframe at
   production;
6. request origin: `X-Forwarded-Host` (falling back to `Host`) +
   `X-Forwarded-Proto`;
7. the local httpuv origin (stdio has no request).

CSP `connectDomains` gets origins only (host sanitizers reject paths); the
full path-aware base feeds the websocket URL. Disable the fast path
entirely with `options(shiny.mcp.direct = FALSE)`.

## One connector, many apps (gateway + appId)

A single MCP connector can front several Shiny apps:

- Each app sets a unique `options(shiny.mcp.appId = "<id>")`
  (`[A-Za-z0-9_-]+`). This prefixes the internal tunnel tools
  (`demo_shiny_connect`, ...) via `mcpTunnelToolName()` and publishes the
  resource as `ui://shiny/<id>` (`mcpResourceUri()`); incoming calls are
  un-prefixed by `mcpTunnelLocalName()` before dispatch. The bridge reads
  `appId` from `__shinyMcpConfig__` and prefixes its `_shiny_*` calls to
  match — the prefix is applied in the single `callTool` closure, so
  socket, XHR and download paths are all covered.
- `mcp/gateway/shiny-mcp-gateway.mjs` (zero-dependency Node; stdio for
  Claude Desktop or `--port` HTTP for basic-host/claude.ai) merges N app
  endpoints: `initialize`/`ping` answered locally, `tools/list` +
  `resources/list` fanned out and merged, `tools/call`/`resources/read`
  routed by name/URI maps (rebuilt on a miss). Everything else — CSP,
  directBase, tool metadata — passes through verbatim, so per-app direct
  connect keeps working. App-facing tool names must be unique across apps.

## Security decisions

- Tunnel tools are `visibility: ["app"]` — the model can't drive a user's
  session.
- No CORS is added to app endpoints; only `/mcp` gets CORS headers.
  HTTP side channels stay tunneled in direct mode for the same reason.
- Deployment records are only trusted on a Host match (no split-brain to
  prod from a local run).
- `mcpSendMessage()` sends `role: "user"` content — the host shows it as
  coming from the user; hosts may prompt.

## Options reference

| Option | Effect |
|---|---|
| `shiny.mcp` | mount `/mcp` (master switch) |
| `shiny.mcp.stdio` | also speak JSON-RPC on stdin/stdout |
| `shiny.mcp.tool` | name/description/inputSchema of the app tool |
| `shiny.mcp.appId` | namespace internal tools + resource URI (gateway) |
| `shiny.mcp.direct` | direct-connect fast path (default TRUE) |
| `shiny.mcp.origin` | explicit external base URL (proxy override) |
| `shiny.mcp.displayModes` | subset of inline/fullscreen/pip |

Author tools are registered via `registerMcpTool(ellmer::tool(...))`, not
through an option.

## File map

| Path | Role |
|---|---|
| `R/mcp-server.R` | endpoint, dispatch, tools/resources, direct-base logic |
| `R/mcp-tunnel.R` | McpConnection, tunnel tools, `_shiny_http` chain |
| `R/mcp-app.R` | single-file resource: inlining, CSS url() rewrite, config |
| `R/mcp-session.R` | exported session API (one Rd page `mcp-session`) |
| `R/mcp-stdio.R` | stdio transport |
| `R/server.R` | wiring: `/mcp` in handler chain, staticPath exclusion, stdio start |
| `R/html-deps.R` | dynamic-dependency inlining hook for MCP sessions |
| `srcts/src/mcp/index.ts` | bridge entry: App handshake, createSocket, config |
| `srcts/src/mcp/hybridSocket.ts` | direct-vs-tunnel selection |
| `srcts/src/mcp/tunnelSocket.ts` | send/receive long-poll socket |
| `srcts/src/mcp/tunnelXhr.ts` | XHR replacement over `_shiny_http` |
| `inst/www/shared/shiny-mcp-bridge.js` | committed bundle (`npm run bundle_mcp`) |
| `mcp/gateway/shiny-mcp-gateway.mjs` | multi-app gateway |
| `tests/testthat/test-mcp-*.R`, `srcts/src/mcp/__tests__/` | tests (TS tests run via `npx tsx --test`) |

## Known limitations / open items

- Claude Code (terminal CLI) calls tools but does not render MCP Apps
  iframes; rendering hosts are Claude Desktop / claude.ai / VS Code
  Copilot / MCPJam / basic-host.
- claude.ai currently hardcodes the iframe CSP
  (anthropics/claude-ai-mcp#40), so the direct fast path is expected to
  fall back to the tunnel there.
- SSE (server push on GET /mcp) is unimplemented — JSON responses only.
- CORS question deferred (QUESTIONS.md §3); mcptools upstreaming filed as
  posit-dev/mcptools#115–#120; stdio unsupported on Windows.
