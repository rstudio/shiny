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

Implementation lives in `R/mcp-server.R`, `R/mcp-tunnel.R`, `R/mcp-app.R`,
`R/html-deps.R` (processDeps hook), `srcts/src/mcp/` (built to
`inst/www/shared/shiny-mcp-bridge.js`), with tests in
`tests/testthat/test-mcp-*.R` and `srcts/src/mcp/__tests__/`.

Known cosmetic gap: `url()` references inside inlined CSS (e.g. FontAwesome
webfonts) 404 in the sandbox, so some icon glyphs don't render.

## Contents

| File | What it is | Source of truth |
|---|---|---|
| `research-notes.md` | Protocol research, Shiny transport seams, the three candidate architectures + recommendation | `.context/mcp-apps-research.md` |
| `design-spec.md` | Approved Phase 1 design | `docs/superpowers/specs/2026-07-10-shiny-mcp-apps-design.md` |
| `implementation-plan-phase1.md` | Phase 1 task-by-task plan (executed) | `docs/superpowers/plans/2026-07-10-shiny-mcp-apps-phase1.md` |
| `implementation-plan-phase2.md` | Phase 2 task-by-task plan (executed) | `docs/superpowers/plans/2026-07-10-shiny-mcp-apps-phase2.md` |
| `demo-app/app.R` | Demo app (slider/plot, upload, download, server-side selectize, dynamic UI) with MCP enabled | `.context/mcp-demo/app.R` |
| `README-mcp-host.md` | How to run the ext-apps basic-host against a Shiny app, with all gotchas | — |
| `run-mcp-host.sh` | Start script for the basic-host (host :8090, sandbox :8081) | — |
| `screenshot-e2e-basic-host.png` | Phase 1 E2E proof: demo app reactive inside the sandboxed iframe | — |
| `screenshot-e2e-phase2.png` | Phase 2 E2E proof: upload + dynamic UI + selectize + theme | — |

## Quick start

```sh
# Terminal 1
Rscript -e 'devtools::load_all(); shiny::runApp("mcp/demo-app", port = 7788)'
# Terminal 2 (needs a prepared ext-apps clone; see README-mcp-host.md)
mcp/run-mcp-host.sh
# Browser
open "http://localhost:8090/?server=shiny&tool=open_shiny_app&call=true"
```

## Backlog (Phase 3+)

User-facing API design (author-declared tools, `session$mcp$…` accessors
instead of `.clientdata_mcp_*` / custom messages), stdio transport for local
desktop hosts, direct-connect `wss://` fast path where hosts honor CSP,
display modes (fullscreen/pip), inlining CSS `url()` assets (fonts/images) as
data URIs, and upstreaming resources/`_meta`/async support to {mcptools}.
