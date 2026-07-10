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
