# Shiny × MCP Apps (SEP-1865) — working docs

Planning documents, research notes, and demo material for Shiny's experimental
MCP Apps support. Collected here for discoverability; the location may move.
Keep these in sync with the sources listed below.

## Status (2026-07-10)

Phases 1 and 2 are implemented and E2E-verified. With `options(shiny.mcp =
TRUE)`, a Shiny app serves an MCP endpoint at `/mcp` on its own port, and MCP
Apps hosts render the live, reactive app in a sandboxed iframe (websocket
traffic tunneled over postMessage → `tools/call`).

- **Phase 1:** endpoint, session tunnel, single-file `ui://shiny/app`
  resource, `Shiny.createSocket` bridge. Inputs/outputs/plots/dynamic UI.
- **Phase 2:** HTTP side channels tunneled via `_shiny_http` (file
  upload/download, server-side DataTables/selectize, dynamically inserted
  htmlDependencies inlined for the sandbox), plus host theme/style variables,
  tool-input/result delivered to `session$clientData`, and
  `session$sendCustomMessage("shiny.mcp.updateModelContext" | "…sendMessage")`
  bridges. Verified in basic-host: upload, dynamic panel, server-side
  selectize, theme toggle (`screenshot-e2e-phase2.png`).

- **CSS assets:** relative `url()` refs inside inlined stylesheets (webfonts,
  background images) are rewritten to `data:` URIs, so icon fonts like
  FontAwesome render in the sandbox (`screenshot-e2e-css-fonts.png`). Legacy
  font fallback formats (ttf/otf/eot) are intentionally left un-inlined —
  browsers never fetch them once the inlined woff2 loads — keeping the
  resource ~2.3 MB instead of ~5 MB.
- **Stdio transport:** `options(shiny.mcp.stdio = TRUE)` additionally speaks
  newline-delimited JSON-RPC over stdin/stdout (non-blocking stdin polled
  from the `later` loop, coexisting with httpuv), so local hosts can launch
  the app process directly:

  ```sh
  claude mcp add shiny-demo -- Rscript -e \
    "options(shiny.mcp=TRUE, shiny.mcp.stdio=TRUE); shiny::runApp('path/to/app', launch.browser=FALSE)"
  ```

  Verified E2E: `claude --print` launched the demo app over stdio and called
  `get_sample_stats` successfully. Caveats: stdout is reserved for the
  protocol (don't `cat()` to it; `message()` is fine), no Windows support
  (non-blocking stdin), and terminal Claude Code still doesn't render the
  iframe UI — tools/resources only.
- **Direct-connect fast path:** the resource declares the app's origin in
  `_meta.ui.csp.connectDomains`, and the bridge tries a real WebSocket
  (`<base>/websocket/?mcp=1`, 2.5 s timeout) before falling back to the
  tunnel. The base URL is **path-aware** so sub-path deployments (Posit
  Connect `/content/<guid>`) connect directly: priority is
  `options(shiny.mcp.origin=)` → Connect's `RStudio-Connect-App-Base-Url`
  header (what Connect actually sends to Shiny content, verified on a real
  deployment; `X-RSC-Request` also honored for API content) →
  shinyapps.io's `X-Redx-Frontend-Name` header (schemeless host + path,
  verified on a real deployment) → a
  host-matched deployment record written at deploy time (rsconnect's
  `rsconnect/**/*.dcf` `url` or Posit Publisher's
  `.posit/publish/deployments/*.toml` `direct_url`; Quarto's `_publish.yml`
  was considered and dropped) → request
  `Host` + `X-Forwarded-Proto` → local httpuv origin (stdio). Deployment
  records are only trusted when their host matches the serving request, so
  local runs of deployed app dirs never point the iframe at production —
  note rsconnect does **not** include its `rsconnect/` records in the
  uploaded bundle, so on the server the header is the working signal. Verified in
  basic-host (`__shinyMcpTransport__ === "direct"`) and against a real
  connect.posit.it deployment. HTTP side channels stay tunneled (no
  CORS exposure) via connectionId-less `_shiny_http`. Disable with
  `options(shiny.mcp.direct = FALSE)`.
- **Display modes:** the app declares
  `options(shiny.mcp.displayModes = ...)` (default: inline, fullscreen,
  pip) and can call `mcpRequestDisplayMode("fullscreen")`; the resulting
  mode arrives via `mcpHostContext()$displayMode`. Unit-tested; basic-host
  doesn't implement display modes, so not yet E2E-verified on a real host.
- **Author-declared tools:** `options(shiny.mcp.tools = list(list(name=,
  description=, inputSchema=, handler=)))` exposes plain R functions as
  model-callable MCP tools alongside the app tool. Handlers run in the
  server R process (no session), may return character/list/promise, and
  errors surface as MCP tool errors. Invalid or name-colliding specs are
  skipped with a warning. Verified E2E in basic-host (`get_sample_stats`
  in the demo app).
- **Multi-app gateway (one connector, many apps):** each app sets a unique
  `options(shiny.mcp.appId = "<id>")`, which namespaces its internal
  `_shiny_*` tools (`demo_shiny_connect`, ...) and publishes its resource
  as `ui://shiny/<appId>`; `gateway/shiny-mcp-gateway.mjs` (zero-dep Node,
  stdio or `--port` HTTP) then merges several app endpoints into a single
  MCP server. E2E verified in basic-host with `demo-app` + `demo-app2`
  behind one gateway, including full tunnel reactivity
  (`screenshot-e2e-gateway.png`); see `gateway/README.md`.
- **Session API (exported, experimental):** `isMcpSession()`,
  `mcpToolInput()` (parsed tool arguments, reactive), `mcpHostContext()`
  (theme/locale/display mode, reactive), `mcpUpdateModelContext(text=, data=)`
  (updates the model's context; no-op outside MCP), and `mcpSendMessage(text)`
  (user-role chat message). Tool arguments are declared via
  `options(shiny.mcp.tool = list(name=, description=, inputSchema=))`.
  See `man/mcp-session.Rd` and `demo-app/app.R`; verified E2E in basic-host
  (`screenshot-e2e-session-api.png`: tool input moves the slider and shows a
  note; the host's Model Context panel tracks the app; `ui/message` logged
  by the host).

Implementation lives in `R/mcp-server.R`, `R/mcp-tunnel.R`, `R/mcp-app.R`,
`R/html-deps.R` (processDeps hook), `srcts/src/mcp/` (built to
`inst/www/shared/shiny-mcp-bridge.js`), with tests in
`tests/testthat/test-mcp-*.R` and `srcts/src/mcp/__tests__/`.

## Contents

| File | What it is | Source of truth |
|---|---|---|
| `research-notes.md` | Protocol research, Shiny transport seams, the three candidate architectures + recommendation | `.context/mcp-apps-research.md` |
| `design-spec.md` | Approved Phase 1 design | `docs/superpowers/specs/2026-07-10-shiny-mcp-apps-design.md` |
| `implementation-plan-phase1.md` | Phase 1 task-by-task plan (executed) | `docs/superpowers/plans/2026-07-10-shiny-mcp-apps-phase1.md` |
| `implementation-plan-phase2.md` | Phase 2 task-by-task plan (executed) | `docs/superpowers/plans/2026-07-10-shiny-mcp-apps-phase2.md` |
| `demo-app/app.R` | Demo app (slider/plot, upload, download, server-side selectize, dynamic UI) with MCP enabled | `.context/mcp-demo/app.R` |
| `demo-app2/app.R` | Second demo app (mtcars explorer, appId `cars`) for the multi-app gateway | — |
| `gateway/shiny-mcp-gateway.mjs` | Zero-dep Node gateway merging several Shiny MCP endpoints into one server | — |
| `gateway/README.md` | Gateway usage + design | — |
| `screenshot-e2e-gateway.png` | Gateway E2E proof: two apps behind one connector | — |
| `README-mcp-host.md` | How to run the ext-apps basic-host against a Shiny app, with all gotchas | — |
| `run-mcp-host.sh` | Start script for the basic-host (host :8090, sandbox :8081) | — |
| `screenshot-e2e-basic-host.png` | Phase 1 E2E proof: demo app reactive inside the sandboxed iframe | — |
| `screenshot-e2e-phase2.png` | Phase 2 E2E proof: upload + dynamic UI + selectize + theme | — |
| `screenshot-e2e-css-fonts.png` | CSS url() inlining proof: FontAwesome glyph renders in the sandbox | — |
| `screenshot-e2e-session-api.png` | Session API proof: tool input + Model Context panel | — |
| `mcptools-upstream-notes.md` | Gaps {mcptools} would need to close to host this (candidate upstream issues) | — |
| `QUESTIONS.md` | Open decisions for Barret, with context | — |

## Quick start

```sh
# Terminal 1
Rscript -e 'devtools::load_all(); shiny::runApp("mcp/demo-app", port = 7788)'
# Terminal 2 (needs a prepared ext-apps clone; see README-mcp-host.md)
mcp/run-mcp-host.sh
# Browser
open "http://localhost:8090/?server=shiny&tool=open_shiny_app&call=true"
```

## Backlog

Upstreaming resources/`_meta`/async support to {mcptools} (see
`mcptools-upstream-notes.md`), and the open decisions in `QUESTIONS.md`.
