# Shiny as an MCP App — Design

Date: 2026-07-10
Status: approved direction (Barret, 2026-07-10): implement within {shiny}, target the
`claude mcp add` workflow, easiest-functional design first; user-facing API polish is a
follow-up.

## Goal

A Shiny app can be exposed as an MCP server implementing the
[MCP Apps extension (SEP-1865)](https://github.com/modelcontextprotocol/ext-apps/blob/main/specification/2026-01-26/apps.mdx),
so that an MCP host (Claude Desktop, claude.ai, VS Code Copilot, MCPJam, the reference
`basic-host`, …) can call a tool and render the **live, running Shiny app** in a sandboxed
iframe inside the conversation, with full reactivity.

Claude Code (terminal CLI) can register the server (`claude mcp add --transport http …`)
and call its tools, but does not render iframes; end-to-end rendering is developed and
verified against the reference `basic-host` from the `ext-apps` repo.

## Constraints that shaped the design (verified)

1. **MCP Apps has no server→client push.** The iframe may only issue requests
   (`tools/call`, `resources/read`) through the host. Shiny's server→client reactive
   messages must be delivered by long-polling a tool call. Shiny's HTTP stack is
   promise-aware (`hybrid_chain`, `R/middleware.R:331-360`), so the R process can hold a
   request without blocking.
2. **The iframe is sandboxed with a deny-by-default CSP**, and claude.ai currently
   hardcodes CSP (ignores `resourceDomains`; `connectDomains` unreliable —
   anthropics/claude-ai-mcp#40). Therefore: the `ui://` resource must be a
   **self-contained single-file HTML** (all JS/CSS inlined), and all communication goes
   over postMessage (no direct websocket).
3. **Client seam:** `Shiny.createSocket` override (`srcts/src/shiny/index.ts:85`,
   `shinyapp.ts:194`) accepts any WebSocket-like object (`send`, `close`, truthy
   `readyState`, `onopen/onmessage/onclose`). Shinylive precedent.
4. **Server seam:** the "websocket" consumed by `createAppHandlers()$ws()` /
   `ShinySession` is duck-typed: `$send(msg)`, `$close()`, `$request`,
   `$onMessage(fn(binary, msg))`, `$onClose(fn)` (`R/server.R:153-315`,
   `R/shiny.R:542,999,1378`).
5. **Plots already work in a sandbox:** `session$fileUrl()` returns base64 `data:` URIs
   (`R/shiny.R:2396-2407`), used by `renderPlot`/`renderImage`.
6. **mcptools 1.0.0 cannot serve MCP Apps**: no `resources/list`/`resources/read`
   handlers (falls through to "Method not found"), no tool `_meta` in `tools/list`,
   synchronous-only tool execution (no long-poll), and `mcp_server()` owns its process /
   port (cannot mount into Shiny's httpuv app). We therefore implement a **minimal MCP
   endpoint inside shiny** (small JSON-RPC surface), mirroring mcptools' security
   validations (origin/host checks), and treat "mcptools grows resources + `_meta` +
   async and shiny delegates to it" as a follow-up.

## Architecture (MVP = Phase 1)

```
MCP host (basic-host / Claude Desktop / claude.ai / VS Code)
   │  streamable HTTP (JSON responses; GET/SSE not offered)
   ▼
Shiny's httpuv server ──────────────── same R process, same port
   ├── POST /mcp            new MCP JSON-RPC handler (promise-aware)
   ├── GET  /…              normal Shiny app (unchanged)
   └── ws   /websocket/     normal websocket (unchanged)

Host renders ui://shiny/app (single-file HTML) in sandboxed iframe:
   [inlined: jquery, shiny.js+css, bootstrap/bslib deps, app <body>, mcp bridge JS]
   bridge: ui/initialize handshake, then Shiny.createSocket → McpTunnelWebSocket
   │  postMessage JSON-RPC (tools/call on app-only tools)
   ▼ host proxies to POST /mcp
R: fake-ws object → createAppHandlers()$ws(fakeWs) → real ShinySession
```

### MCP endpoint (R, new `R/mcp-server.R`)

Mounted in the per-app handler chain from `createAppHandlers()` (before `uiHttpHandler`)
when MCP is enabled; path `/mcp` also excluded from static serving (like `session`).

JSON-RPC methods (streamable HTTP, `application/json` responses; `GET /mcp` → 405,
which the spec permits — clients MUST accept plain JSON responses):

- `initialize` — protocol version negotiation (mirror mcptools), capabilities
  `{tools: {listChanged: false}, resources: {subscribe: false, listChanged: false}}`,
  serverInfo.
- `notifications/*` — 202 empty.
- `ping` — `{}`.
- `tools/list` — the visible app tool + hidden tunnel tools (marked
  `_meta.ui.visibility: ["app"]`; spec-compliant hosts hide them from the model).
- `tools/call` — dispatch (may return a promise; long-poll lives here).
- `resources/list` / `resources/read` — the `ui://shiny/app` resource.

Security: mirror mcptools' host/origin validation (localhost origins + absent Origin
allowed by default). CORS headers + OPTIONS preflight on `/mcp` (browser-based hosts
like basic-host require it; the ext-apps guide uses `cors()`).

### Tools

| Tool | Visibility | Purpose |
|---|---|---|
| `open_shiny_app` (name/description configurable via options) | model + app | Returns text like "The Shiny app is displayed"; carries `_meta.ui.resourceUri: "ui://shiny/app"`. |
| `_shiny_connect` | app | Creates fake-ws + `ShinySession` via `createAppHandlers()$ws(fakeWs)`; returns `{connectionId}` (random token). |
| `_shiny_send` | app | `{connectionId, frames: [{data, binary?}]}` → invokes fake-ws `onMessage`; response piggybacks any queued outbound frames. |
| `_shiny_receive` | app | Long-poll: resolves immediately if outbound frames queued, else holds a promise until frames arrive or ~15 s timeout (empty batch). Client re-polls in a loop. |
| `_shiny_close` | app | Tears down the session (also GC'd after inactivity). |

Binary frames base64-encoded with `binary: true`.

### The `ui://shiny/app` resource (R, new `R/mcp-app.R`)

Rendered once per read: run the app's UI (same path as `uiHttpHandler`), resolve
htmlDependencies, and emit a single HTML document with every file-based script/stylesheet
inlined (`</script` escaped as `<\/script`), plus `inst/www/shared/shiny-mcp-bridge.js`
inlined last. `_meta.ui` on the resource: none required for MVP (defaults suffice;
data: images are allowed by the default CSP).

Known MVP limitations (Phase 2 closes them): file upload, `downloadButton`,
DataTables/selectize server-side ajax, and dynamically inserted htmlDependencies that
aren't on the initial page — these use same-origin HTTP and need the `_shiny_http`
tunnel + a srcts fetch indirection. CSS `url()` references (fonts/images) inside inlined
stylesheets will not resolve; acceptable for MVP.

### Client bridge (TS, new `srcts/src/mcp/` → `inst/www/shared/shiny-mcp-bridge.js`)

- Uses `@modelcontextprotocol/ext-apps`'s `App` class (new npm devDependency, MIT,
  bundled by esbuild — guarantees wire compatibility with hosts' AppBridge/sandbox
  proxy) for the postMessage JSON-RPC handshake (`ui/initialize` →
  `ui/notifications/initialized`) and `callServerTool`.
- `McpTunnelWebSocket`: WebSocket-shaped class (modeled on Shinylive's
  MessagePortWebSocket). `send()` → `_shiny_send`; a receive loop drives
  `_shiny_receive`; frames delivered via `onmessage`. `close()` → `_shiny_close`.
- Installs itself: `window.Shiny.createSocket = () => new McpTunnelWebSocket(app)`
  (bridge script is inlined after shiny.js, before DOM-ready init).
- Handles `ui/resource-teardown` (respond, then close). Ignores
  `tool-input`/`tool-result` notifications for MVP.
- New esbuild entry in `srcts/build/` producing the bundle.

### Enabling (MVP, no committed API)

`mcpConfigure()` (called before the app starts) enables the `/mcp` handler
and resource. Optional `mcpConfigure(description=, arguments=)`. Marked
experimental; a real user-facing API (tool schemas, `session$mcp$…`,
`ui/update-model-context`) is the agreed follow-up.

## Phasing

- **Phase 1 (this work):** everything above. Success = a slider+plot app renders and
  reacts inside `basic-host`, and `claude mcp add --transport http` can list/call the
  tool from Claude Code.
- **Phase 2:** `_shiny_http` tunnel + srcts fetch indirection (uploads, downloads,
  dataobj ajax, dynamic dependencies), teardown/reconnect polish, host theming
  (CSS vars → bslib), tool-input → session, `ui/update-model-context` session API.
- **Phase 3:** direct-connect fast path (`csp.connectDomains` + `wss://`) where hosts
  honor CSP; stdio transport; display modes; author-defined tools (ellmer/mcptools
  interop).

## Error handling

- JSON-RPC errors per spec (-32600/-32601/-32603, tool errors as `isError` results).
- Unknown `connectionId` → tool error (iframe shows disconnected state; Shiny's
  existing disconnect overlay applies via `onclose`).
- Long-poll never errors on timeout — returns empty frame batch.
- Session GC: connection registry entry closed after 60 s without any tunnel activity
  (poll cadence is ≤15 s, so healthy clients never trip it).

## Testing

- **R (testthat):** JSON-RPC handler unit tests with synthetic Rook requests
  (initialize/tools/list/resources/read shapes); tunnel lifecycle test — connect, send
  `init` frame, long-poll receives `config` + rendered values; promise resolution via
  `later::run_now()` pumping.
- **TS:** unit test for McpTunnelWebSocket state machine (mock App).
- **E2E (manual/scripted):** demo app + ext-apps `basic-host`, driven by browser
  automation: render, move slider, observe plot update.
