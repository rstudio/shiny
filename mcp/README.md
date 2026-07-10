# Shiny × MCP Apps (SEP-1865) — working docs

Planning documents, research notes, and demo material for Shiny's experimental
MCP Apps support. Collected here for discoverability; the location may move.
Keep these in sync with the sources listed below.

## Status (2026-07-10)

Phase 1 is implemented and E2E-verified: with `options(shiny.mcp = TRUE)`, a
Shiny app serves an MCP endpoint at `/mcp` on its own port, and MCP Apps hosts
render the live, reactive app in a sandboxed iframe (websocket traffic
tunneled over postMessage → `tools/call`). See `screenshot-e2e-basic-host.png`
for the verified run in the reference basic-host.

Implementation lives in `R/mcp-server.R`, `R/mcp-tunnel.R`, `R/mcp-app.R`,
`srcts/src/mcp/` (built to `inst/www/shared/shiny-mcp-bridge.js`), with tests
in `tests/testthat/test-mcp-*.R`.

## Contents

| File | What it is | Source of truth |
|---|---|---|
| `research-notes.md` | Protocol research, Shiny transport seams, the three candidate architectures + recommendation | `.context/mcp-apps-research.md` |
| `design-spec.md` | Approved Phase 1 design | `docs/superpowers/specs/2026-07-10-shiny-mcp-apps-design.md` |
| `implementation-plan-phase1.md` | Task-by-task implementation plan (executed) | `docs/superpowers/plans/2026-07-10-shiny-mcp-apps-phase1.md` |
| `demo-app/app.R` | Minimal demo app (slider + plot + text) with MCP enabled | `.context/mcp-demo/app.R` |
| `README-mcp-host.md` | How to run the ext-apps basic-host against a Shiny app, with all gotchas | — |
| `run-mcp-host.sh` | Start script for the basic-host (host :8090, sandbox :8081) | — |
| `screenshot-e2e-basic-host.png` | E2E proof: demo app reactive inside the sandboxed iframe | — |

## Quick start

```sh
# Terminal 1
Rscript -e 'devtools::load_all(); shiny::runApp("mcp/demo-app", port = 7788)'
# Terminal 2 (needs a prepared ext-apps clone; see README-mcp-host.md)
mcp/run-mcp-host.sh
# Browser
open "http://localhost:8090/?server=shiny&tool=open_shiny_app&call=true"
```

## Phase 2 backlog

HTTP side-channel tunnel (`_shiny_http`: uploads, downloads, DataTables/
selectize, dynamic htmlDependencies), host theming (CSS vars → bslib),
tool-input → session reactive, `ui/update-model-context` session API,
user-facing API design, stdio transport, direct-connect `wss://` fast path
where hosts honor CSP, upstreaming resources/`_meta`/async support to
{mcptools}.
