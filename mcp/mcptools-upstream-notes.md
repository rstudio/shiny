# What {mcptools} would need to serve MCP Apps (upstream notes)

Barret's original preference was to build Shiny's MCP support on
{posit-dev/mcptools}. That wasn't possible (verified against v1.0.0 and the
dev branch on 2026-07-10), so shiny grew a minimal internal endpoint
(`R/mcp-server.R`). This file documents the gaps as candidate upstream
issues/PRs, so the two implementations can converge later.

## Gaps in mcptools (as of 1.0.0.9000)

1. **No resources support.** `resources/list` and `resources/read` fall
   through to `Method not found` (`R/server.R`,
   `handle_http_request_message()` / `handle_message_from_client()`), even
   though `capabilities()` advertises `resources`. MCP Apps requires
   `resources/read` to deliver the `ui://` HTML.

2. **No tool `_meta`.** `tool_as_json()` emits
   name/title/description/inputSchema/annotations only. MCP Apps needs
   `_meta.ui.resourceUri` on the app tool and `_meta.ui.visibility` on
   app-only tools; ellmer's tool annotations have no place to carry
   arbitrary `_meta`.

3. **Synchronous-only tool execution.** `execute_tool_call()` returns
   immediately; the HTTP handler returns plain lists. Shiny's tunnel
   requires long-poll semantics — a tool call whose response resolves
   minutes later (promises + later). mcptools' httpuv `call` would need to
   support returning promises (httpuv itself does).

4. **Process-owning design.** `mcp_server()` blocks (stdio: nanonext
   `wait(cv)` loop; http: own `httpuv::startServer` + `service(Inf)`), and
   `check_not_interactive()` forbids embedding. Shiny needed the endpoint
   *mounted into an existing httpuv app* and driven by an existing event
   loop. An exported "give me a JSON-RPC dispatch function" seam
   (essentially what shiny's internal `mcpDispatch()` is) would let any
   httpuv-based framework (shiny, plumber2, ambiorix) embed mcptools.

5. **Stdio + event-loop coexistence.** mcptools' stdio loop blocks the
   process. Shiny's `R/mcp-stdio.R` shows a dependency-free alternative:
   `file("stdin", blocking = FALSE)` polled from a `later` loop (Unix only).
   nanonext's `read_stdin()` could do the same non-blockingly if mcptools
   exposed it as a pollable rather than a blocking loop.

## Suggested upstream shape

```r
# mcptools could export:
dispatch <- mcp_dispatch(tools = ..., resources = ...)  # function(body, req) -> list | promise
# so embedders do:
#   response <- dispatch(jsonrpc_body)         # shiny/plumber mount this
# and mcp_server() becomes a thin stdio/http wrapper around it.
```

Plus: `mcp_resource(uri, mime_type, handler, meta = NULL)` and a `meta`
argument on served tools.

## Status

Not yet filed upstream — see `QUESTIONS.md` (whether to open these as
issues on posit-dev/mcptools, and whether shiny should eventually delegate
its endpoint to mcptools once the gaps close).
