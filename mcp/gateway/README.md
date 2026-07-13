# shiny-mcp-gateway — one MCP connector, many Shiny apps

`shiny-mcp-gateway.mjs` is a small zero-dependency Node script that merges
several Shiny MCP endpoints into a single MCP server, so one Claude
connector (Desktop entry or claude.ai custom connector) exposes several
apps.

## Requirements

Each backend app must set a **unique** `options(shiny.mcp.appId = "<id>")`
(letters/digits/`_`/`-`). This namespaces the app's internal `_shiny_*`
tunnel tools (`demo_shiny_connect`, ...) and publishes its UI resource as
`ui://shiny/<appId>`, so the gateway can route each iframe's traffic to the
right app. App-facing tool names (`options(shiny.mcp.tool/tools)`) must
also be unique across apps.

## Usage

```sh
# stdio (Claude Desktop config entry):
node shiny-mcp-gateway.mjs URL [URL...]

# HTTP (basic-host, MCPJam, claude.ai custom connector):
node shiny-mcp-gateway.mjs --port 7790 URL [URL...]
```

where each URL is an app's MCP endpoint, local or deployed, e.g.

```sh
node shiny-mcp-gateway.mjs --port 7790 \
  http://127.0.0.1:7788/mcp \
  https://barret.shinyapps.io/shiny-mcp-demo/mcp
```

Claude Desktop entry (`claude_desktop_config.json`):

```json
"shiny-apps": {
  "command": "/opt/homebrew/bin/node",
  "args": [
    "/path/to/shiny-mcp-gateway.mjs",
    "https://barret.shinyapps.io/shiny-mcp-demo/mcp",
    "https://barret.shinyapps.io/shiny-mcp-cars/mcp"
  ]
}
```

## How it works

- `initialize`/`ping` are answered locally; `tools/list` and
  `resources/list` fan out to all backends and merge (duplicates are
  dropped with a warning on stderr).
- `tools/call` and `resources/read` are routed by tool name / resource URI
  using maps built from the merged lists (refreshed on a miss).
- Everything else about the protocol passes through verbatim — per-app
  `_meta.ui.csp` and `directBase` still point at each app's own URL, so
  the direct-connect fast path keeps working per app on hosts that honor
  CSP.

## E2E verified (2026-07-13)

basic-host → gateway (`--port 7790`) → `mcp/demo-app` (appId `demo`) +
`mcp/demo-app2` (appId `cars`): merged 13 tools + 2 resources; both apps
render via `open_shiny_app` / `open_cars_app`; the cars app ran with
`shiny.mcp.direct=FALSE` to prove full reactivity through the gateway over
the namespaced tunnel tools (selectize change re-rendered plot + cor text).
See `../screenshot-e2e-gateway.png`.
