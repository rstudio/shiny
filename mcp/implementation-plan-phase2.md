# Shiny MCP Apps (Phase 2) Implementation Plan

> **Status: in progress 2026-07-10.** Executed inline by the same agent that did Phase 1.

**Goal:** Close the HTTP side-channel gaps (uploads, downloads, server-side DataTables/selectize, dynamic htmlDependencies) and integrate host features (theme/styles, tool-input, model-context updates) so realistic Shiny apps work inside the MCP Apps sandbox.

**Architecture:** One new app-only tool `_shiny_http` invokes the app's existing Rook handler chain in-process (promise-aware). On the client, the bridge swaps `window.XMLHttpRequest` for a tunnel implementation inside the sandboxed iframe — jQuery's ajax (uploads, DataTables, selectize) and raw XHR (restyle) all route through it with no shiny.js changes. Dynamic htmlDependencies are inlined **server-side** for MCP sessions via a `processDeps()` hook (the client executes `dep.head` HTML, render.ts:498). Downloads are intercepted at the `a.shiny-download-link` click and delivered as blob URLs. Host theme/styles apply via the SDK helpers; tool-input lands in `session$clientData`; `session$sendCustomMessage("shiny.mcp.updateModelContext", ...)` / `"shiny.mcp.sendMessage"` bridge to `app.updateModelContext()` / `app.sendMessage()`.

## Global Constraints

- Same as Phase 1 (no new R deps, no exported R API; options-gated; frames base64).
- `_shiny_http` args: `{connectionId, method, path, headers?, body?}` (`body` base64; `path` may include query string). Result `structuredContent`: `{status, headers, body}` (`body` base64). Requires a live `connectionId`.
- The tunnel handler chain excludes the `/mcp` endpoint itself (no recursion): `sessionHandler + app httpHandlers + sys.www.root + resourcePathHandler`.
- MCP sessions are detected via `HTTP_MCP_TUNNEL = "1"` on the fake ws request.
- Custom message types: `shiny.mcp.updateModelContext`, `shiny.mcp.sendMessage`.
- Tool input/result surface as `session$clientData$mcp_tool_input` / `mcp_tool_result` (JSON-encoded string values via `.clientdata_*` inputs).

### Task 1: `_shiny_http` tool (R)

- `mcpRookInput(bytes)`: stateful `read(l = -1)` (consumes; `raw(0)` at EOF), `rewind()`, `read_lines()` stub — the upload handler loops `read(2^16)` until empty, so Phase 1's replay-everything fake would infinite-loop.
- `McpConnection$new()` request gains `HTTP_MCP_TUNNEL = "1"`.
- `mcpTunnelToolCall()` gains `_shiny_http`, with a `tunnelHttpHandler` built in `createAppHandlers()` and threaded through `mcpHttpHandler(uiHandler, wsHandler, tunnelHttpHandler)`.
- Request env: `REQUEST_METHOD`, `PATH_INFO`/`QUERY_STRING` split from `path`, `CONTENT_TYPE`/`CONTENT_LENGTH` + `HTTP_*` from headers, `rook.input`.
- Response serialization handles `content` as character, raw, and `list(file=, owned=)` (read + optionally delete).
- Tests: chunked rook input; static asset GET via chain; 404; `registerDownload` → `_shiny_http` GET returns bytes + `Content-Disposition` (uses a real tunnel session).

### Task 2: dynamic dependency inlining (R)

- `isMcpSession(session)`: checks `session$request$HTTP_MCP_TUNNEL`.
- `mcpInlineDependency(dep)`: file-based scripts/stylesheets → `head` inline HTML (reuse `readAssetText`/`escape*Content`); keeps existing `head`; drops `meta`/`attachment`; falls back to `createWebDependency()` when no `src$file`.
- `processDeps()` picks the transform based on `isMcpSession(session)`.
- Tests: inline transform shape; processDeps with a real tunnel session produces head-inlined deps; non-MCP session unchanged.

### Task 3: TunnelXMLHttpRequest + downloads (TS)

- `srcts/src/mcp/tunnelXhr.ts`: minimal XHR surface used by jQuery 3 + render.ts restyle (`open/setRequestHeader/send/abort/getAllResponseHeaders/getResponseHeader/overrideMimeType`, `readyState/status/statusText/responseText/response/responseType`, `onload/onerror/onabort/ontimeout/onreadystatechange`). Tunnels same-origin/relative URLs via `_shiny_http`; delegates absolute foreign URLs to the native XHR. Body support: string, Blob/File (→ ArrayBuffer → base64), ArrayBuffer/TypedArray.
- Bridge installs it (`window.XMLHttpRequest = makeTunnelXhr(...)`) once the socket connects.
- Download interception: capture-phase click listener on `a.shiny-download-link` → preventDefault → tunnel GET → blob URL + `<a download>` click (filename from `Content-Disposition`).
- Tests: node test for TunnelXhr GET/POST/binary/headers/error with a mock caller.

### Task 4: host integration (TS)

- On connect + `onhostcontextchanged`: `applyDocumentTheme(theme)`, `applyHostStyleVariables(styles)`, `applyHostFonts(styles.css.fonts)` (SDK helpers).
- `app.ontoolinput` / `app.ontoolresult` → `Shiny.setInputValue(".clientdata_mcp_tool_input" / "_result", JSON string)` (queued until Shiny connects).
- `Shiny.addCustomMessageHandler("shiny.mcp.updateModelContext", ...)` → `app.updateModelContext(msg)`; same for `sendMessage`.

### Task 5: rebuild, full test pass

### Task 6: E2E in basic-host

Demo app grows: `fileInput` (+ uploaded-file preview), `downloadButton` (CSV), `selectizeInput` with `updateSelectizeInput(server = TRUE)`, dynamic UI via `insertUI` carrying a new dependency. Verify each in the rendered iframe; verify theme toggle (host dark-mode button) restyles the app.

### Task 7: docs — NEWS update, mcp/ mirror refresh, commit
