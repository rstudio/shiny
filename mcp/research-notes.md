# Shiny × MCP Apps — Research Notes & Proposed Plans

Date: 2026-07-10. Status: research complete, awaiting plan selection.

## 1. MCP Apps protocol (SEP-1865, spec 2026-01-26)

Docs: https://modelcontextprotocol.io/extensions/apps/overview
Spec: https://github.com/modelcontextprotocol/ext-apps/blob/main/specification/2026-01-26/apps.mdx
SDK: `@modelcontextprotocol/ext-apps` (MIT, v1.7.4)

### Architecture
- An MCP server registers a **tool** with `_meta.ui.resourceUri: "ui://..."` and a
  **resource** (`mimeType: "text/html;profile=mcp-app"`) whose text/blob is a full HTML5 doc.
- Host fetches the resource via `resources/read` and renders it in a **sandboxed iframe**
  (web hosts: double-iframe sandbox-proxy architecture, different origin, raw HTML injected
  via `ui/notifications/sandbox-resource-ready`).
- App ↔ host speak **JSON-RPC over postMessage** ("dialect of MCP"):
  - App→host requests: `ui/initialize`, `tools/call`, `resources/read`, `ui/open-link`,
    `ui/message`, `ui/request-display-mode`, `ui/update-model-context`, `ping`.
  - Host→app notifications: `ui/notifications/tool-input[-partial]`, `tool-result`,
    `tool-cancelled`, `host-context-changed`; request: `ui/resource-teardown`.
  - App→host notifications: `ui/notifications/size-changed`, `notifications/message`,
    `ui/notifications/initialized`.
- **No server-initiated push to the app.** All server communication is request/response
  routed through the host (`tools/call`, `resources/read`). Push must be emulated
  (long-poll via a held `tools/call`).
- Tool `visibility: ["model","app"]` — `["app"]`-only tools are hidden from the model and
  callable only by the iframe. Host MUST reject app calls to tools without `"app"`.
- CSP via resource `_meta.ui.csp`: `connectDomains` (fetch/XHR/**WebSocket** — `wss://`
  scheme must be declared explicitly), `resourceDomains` (scripts/img/css/fonts),
  `frameDomains`, `baseUriDomains`. Default when omitted: no external connections;
  `img-src 'self' data:` allowed (data: URIs work).
- Host context: theme, CSS variables, displayMode (inline/fullscreen/pip),
  containerDimensions; app reports size via `size-changed`.
- Capability negotiation: client advertises `extensions["io.modelcontextprotocol/ui"]`
  with supported mimeTypes in MCP `initialize`.

### Host-reality caveats (July 2026)
- **claude.ai currently ignores/hardcodes CSP** (anthropics/claude-ai-mcp#40):
  `frame-src 'self' blob: data:` hardcoded; `resourceDomains`/`frameDomains` ignored,
  `connectDomains` only partially respected. wss:// unverified. ChatGPT, MCPJam,
  basic-host honor the spec. ⇒ single-file inlined HTML is mandatory for portability;
  direct network from the iframe cannot be relied on today.
- Claude (web + desktop) consumes MCP Apps via **remote custom connectors**
  (streamable HTTP); local dev = `cloudflared` tunnel. Testing hosts: `basic-host`
  from ext-apps repo (`SERVERS='["http://localhost:3001/mcp"]' npm start`), MCPJam.
- Hosts may require user consent for app-initiated `tools/call`; every tunneled frame is
  a loggable JSON-RPC message (auditability by design).

## 2. Shiny transport seams (from codebase exploration)

### Client (srcts)
- **`Shiny.createSocket`** (srcts/src/shiny/index.ts:85, consumed shinyapp.ts:194-196) is a
  sanctioned override returning a WebSocket-like object. Required surface: `send()`,
  `close()`, `readyState` (truthy=open), `binaryType`, settable `onopen/onmessage/onclose`,
  optional `allowReconnect`. Frames: JSON strings or ArrayBuffer (binary blob framing in
  `makeRequest`, shinyapp.ts:405-470). Shinylive proves this seam (MessagePortWebSocket).
- **HTTP side-channels that bypass the socket** (must be shimmed for sandboxed iframe):
  - upload POST `$.ajax` (srcts/src/file/fileProcessor.ts:185-218; url from `uploadInit` RPC)
  - DataTables server-side ajax (bindings/output/datatable.ts:88-96 → `session/*/dataobj/*`)
  - selectize remote GET (bindings/input/selectInput.ts:171)
  - image `src` (bindings/output/image.ts:110-127) — renderPlot/renderImage default to
    base64 `data:` URLs already; custom `session/*` srcs exist
  - download links (plain href)
  - async render dependency fetch via XHR (shiny/render.ts:255)
- Existing: `window.parent.postMessage("disconnected","*")` (shinyapp.ts:307-311); subApp
  iframes (`R/server.R:358-382` `addSubApp`, deferred iframes).
- Build: esbuild via `tsx srcts/build/shiny.ts`, entry srcts/src/index.ts →
  inst/www/shared/shiny(.min).js; jquery externalized to window.jQuery.

### Server (R)
- httpuv app built in `HandlerManager$createHttpuvApp()` (R/middleware.R:309-368):
  `call` (HTTP), `onWSOpen = wsHandlers$invoke(ws)` (R/middleware.R:364).
- `createAppHandlers()` (R/server.R:134-321): `ws(ws)` handler does
  `ShinySession$new(ws)` + `ws$onMessage/onClose` wiring.
- **The R transport contract is duck-typed**: session/handlers use only
  `ws$send(msg)`, `ws$close()`, `ws$request`, `ws$onMessage(fn(binary,msg))`,
  `ws$onClose(fn)` (R/shiny.R:542, 1378, 999; R/server.R:306-315). A fake ws can be fed
  straight to `createAppHandlers(...)$ws(fakeWs)`. `MockShinySession` proves detachment.
- **HTTP handler stack is invokable directly**: `handlers$invoke(req)` with Rook-style req
  → `httpResponse`; session sub-paths in `ShinySession$handleRequest` (R/shiny.R:2248-2394):
  `file/`, `upload/` (POST, chunked rook.input), `download/`, `dataobj/`.
- **Async-ready**: HTTP path wraps handlers in `hybrid_chain` (R/middleware.R:331-360) —
  handlers may return promises; httpuv supports deferred responses ⇒ long-poll works.
- Client→server frames: `{method:"init",data}`, `{method:"update",data}`,
  RPC `{method,args,tag,blobs}` (binary signature 0x01020202 + length-prefixed blobs).
  Server→client: `{config:{workerId,sessionId,user}}`, `{values},{errors},{inputMessages},
  {custom},{response}` + binary custom messages.
- Message decode: `decodeMessage()` R/server.R:100-127.
- Sessions keyed by 16-hex token in `appsByToken`; side-channel URLs embed token + `?w=workerId`.

### Prior art
- **Shinylive**: `Shiny.createSocket` override + `MessagePortWebSocket` + HTTP proxying over
  MessagePort (`messageporthttp.ts`); uses a Service Worker for asset/HTTP proxying
  (not available in MCP Apps sandbox — no allow-same-origin/SW registration for the view).
- **Shiny Server SockJS**: transport historically replaced *outside* shiny core at the
  onWSOpen/static-index level.
- **mcptools** (posit-dev): R MCP server/client, stdio + HTTP transports (v0.2.0+),
  deployable on Connect. Candidate dependency or reference for JSON-RPC plumbing.

## 3. Proposed plans

### Plan A — Direct connect (CSP-based, no tunnel)
ui:// resource = single-file inlined HTML (rendered app page + deps) + small MCP bridge;
declare `csp.connectDomains: ["https://<origin>","wss://<origin>"]`; shiny.js connects its
normal WebSocket to the app's public origin; all HTTP side-channels hit origin directly
(add CORS headers for sandboxed null-origin; absolute URLs via injected base/URL rewrite).
- \+ Minimal engineering; full fidelity (uploads, DT, downloads); low latency.
- − Requires publicly reachable app; **blocked on claude.ai today** (CSP hardcoding);
  per-host variance forever.

### Plan B — Full postMessage tunnel (the shim; works everywhere)
Client: `Shiny.createSocket` → fake WebSocket over the app bridge; outbound frames batched
into `tools/call` on hidden (`visibility:["app"]`) tools; inbound via long-poll
`tools/call shiny.receive` (R holds promise until flush/timeout ~25s, then app re-polls).
New client-side transport indirection in srcts routes all HTTP side-channels
(upload/dataobj/selectize/download/dependency-XHR) through a fetch-hook →
`shiny.http` tool → R `handlers$invoke(rookReq)` (reuses entire middleware stack).
Downloads → blob URLs; plots stay data: URIs. Hidden tool set (namespaced per app):
`shiny.connect` (→ fake ws + ShinySession, returns session token),
`shiny.send {session, frames[]}`, `shiny.receive {session}` (long-poll),
`shiny.http {session, method, path, headers, body_b64}`, `shiny.close`.
R: minimal MCP endpoint (initialize/tools/list/resources/list/read/tools/call) mounted at
`/mcp` on shiny's own httpuv via handlerManager (streamable HTTP, JSON responses).
ui:// resource = single-file bundle (portable on claude.ai today).
- \+ Host-agnostic (pure postMessage), spec-pure, no public URL beyond the MCP connection,
  auditable; same design portable to py-shiny.
- − Most engineering; every interaction round-trips app→host→server (long-poll adds
  latency to push); host consent/rate-limiting of tools/call is a risk; large payloads
  (base64 plots) may hit host message-size limits (chunking needed).

### Plan C — Sidecar adapter (zero core changes)
Separate process/package (TS or R) speaking MCP to the host, proxying tunneled traffic to
an **unmodified** Shiny app (real ws + HTTP client against the app), serving a rewritten
index.html (injected shim JS) as the ui:// resource.
- \+ Works with any deployed Shiny app (R or Python) immediately; no core risk.
- − Extra hop/process; session affinity/lifecycle management; not "native"; client shim
  JS still needed, and without srcts changes it must monkeypatch ($.ajax/XHR) rather than
  use clean seams.

### Recommendation
Native, phased hybrid: **Plan B as the core**, with the client transport built as a clean
abstraction so **Plan A's direct-connect becomes a feature-detected fast path** later.
- Phase 1 (MVP): R MCP endpoint on httpuv + fake-ws session + bridge JS + fake WebSocket;
  single-file ui:// bundle; inputs/outputs/plots/dynamic-UI work. Test against basic-host.
- Phase 2: HTTP side-channel completeness (srcts fetch-hook: uploads, DT/selectize,
  downloads, dynamic dependencies), teardown/reconnect semantics, theming
  (host CSS vars → bslib), `ui/update-model-context` + `ui/message` session API,
  tool-input → session reactive.
- Phase 3: direct-connect fast path (connectDomains + wss detection), stdio transport for
  local desktop hosts, display modes, resources-based asset loading.

### Open decisions (need user input)
1. **Packaging**: inside {shiny} vs companion package (e.g. {shinymcp}) with shiny
   exporting the seams (fake-ws session ctor, handlers$invoke access, bridge JS asset)?
2. **First target host**: claude.ai remote connector (streamable HTTP) vs local
   Claude Desktop (stdio) vs basic-host-only MVP?
3. **JSON-RPC plumbing**: implement minimal in-repo vs depend on {mcptools}?
4. **Author-facing API**: how apps declare the model-visible tool(s)
   (name/description/inputSchema) and consume tool input + push model context
   (e.g. `session$mcp$onToolInput()`, `session$mcp$updateModelContext()`).
5. Vendor `@modelcontextprotocol/ext-apps` App class vs hand-rolled minimal bridge
   (protocol surface used is small; MIT licensed either way).

### Security notes
- Tunneled requests bypass network-layer auth; hidden tools must validate the session
  token maps to the calling app instance; sessions are unguessable 16-hex tokens.
- Host sees all traffic (by design, auditable). Document that inputs flow through host.
- `shiny.http` must be constrained to the session's own sub-paths + app static assets
  (no open proxy).
