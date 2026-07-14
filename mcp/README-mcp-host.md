# MCP Apps test host for Shiny

How to see a Shiny app render as an MCP App using the reference `basic-host`
from `modelcontextprotocol/ext-apps`.

## One-time setup: prepare the ext-apps clone

```sh
git clone https://github.com/modelcontextprotocol/ext-apps .context/ext-apps
cd .context/ext-apps
npm install --ignore-scripts          # root install; NOT inside examples/basic-host
npm run build -w examples/basic-host  # build just this workspace
```

If the clone lives under a dot-directory (like `.context/`), patch
`examples/basic-host/serve.ts` (~line 127):

```ts
// before (fails: express/send dotfiles policy rejects absolute paths
// containing a dot-directory segment):
res.sendFile(join(DIRECTORY, "sandbox.html"));
// after:
res.sendFile("sandbox.html", { root: DIRECTORY });
```

An already-prepared clone may exist at `.context/ext-apps` (gitignored;
re-create it with the steps above if missing).

## Run it

Terminal 1 — the demo Shiny app (from the repo root):

```sh
Rscript -e 'devtools::load_all(); shiny::runApp("mcp/demo-app", port = 7788)'
```

Terminal 2 — the host:

```sh
mcp/run-mcp-host.sh                      # defaults to :7788/mcp
# or: mcp/run-mcp-host.sh http://127.0.0.1:9999/mcp
```

Then open <http://localhost:8090/?server=shiny&tool=open_shiny_app&call=true>
(or plain <http://localhost:8090> and click Call Tool).

## Showing the app in a real Claude session

Claude Code (terminal) does **not** render MCP Apps iframes — use Claude
Desktop (local) or claude.ai (deployed).

### Claude Desktop (local, stdio)

Add the app as a local stdio server in
`~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "shiny-demo": {
      "command": "Rscript",
      "args": [
        "-e",
        "setwd('/ABSOLUTE/PATH/TO/shiny-repo'); pkgload::load_all(quiet = TRUE); options(shiny.mcp = TRUE, shiny.mcp.stdio = TRUE); shiny::runApp('mcp/demo-app', launch.browser = FALSE)"
      ]
    }
  }
}
```

Then fully restart Claude Desktop (Cmd-Q, not just close the window) and ask
it to *"open the shiny app"* — it calls the `open_shiny_app` tool and the app
renders inline in the conversation.

Notes:

- `pkgload::load_all()` is needed until this branch is installed as
  {shiny}; once installed, replace it with `library(shiny)`.
- Claude Desktop spawns servers without your login-shell `PATH` — if
  `Rscript` isn't found, use its absolute path (`which Rscript`).
- BOTH options are required: `shiny.mcp` enables the endpoint,
  `shiny.mcp.stdio` speaks the protocol on stdin/stdout.
- Never `cat()`/`print()` to stdout in the app at top level — stdout is the
  protocol channel (`message()` is fine).
- R startup can exceed host connect timeouts; for the `claude` CLI use
  `MCP_TIMEOUT=120000`. Pre-warm `pkgload::load_all()` once so compilation
  isn't paid at connect time.

### claude.ai (web, deployed)

Deploy the app (with `options(shiny.mcp = TRUE)` set in app.R / .Rprofile)
somewhere with a public HTTPS URL — e.g. Posit Connect — then in claude.ai:
Settings → Connectors → *Add custom connector* → URL
`https://<your-app-url>/mcp`. The direct-connect fast path may be blocked
there (claude.ai currently hardcodes the iframe CSP,
anthropics/claude-ai-mcp#40); the postMessage tunnel still works.

## Gotchas

- `SANDBOX_PORT` must stay 8081 — the sandbox URL is hardcoded in the built
  client (`examples/basic-host/src/implementation.ts`). `HOST_PORT` is
  flexible (default here 8090; :8080 tends to be occupied locally).
- Don't `npm install` inside `examples/basic-host` (monorepo prepare script
  needs husky/bun); install at the ext-apps root with `--ignore-scripts`.
- `npm start` in basic-host needs bun; `run-mcp-host.sh` uses `npx tsx serve.ts`.
- Zero-setup alternative: `npx @mcpjam/inspector` and paste the /mcp URL.
- Claude Code (terminal) can register the server
  (`claude mcp add --transport http my-shiny-app http://127.0.0.1:7788/mcp`)
  and call tools, but does not render MCP Apps iframes. Rendering hosts:
  Claude / Claude Desktop, VS Code Copilot, MCPJam, basic-host.
