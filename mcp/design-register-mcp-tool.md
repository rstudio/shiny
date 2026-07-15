# Design: `registerMcpTool()` — ellmer-native author tools

**Date:** 2026-07-15
**Branch:** `schloerke/shinymcp-tool-registration` (targets `mcp`)
**Status:** Approved for planning

## Problem

Shiny's MCP author tools — plain R functions the model may call directly —
are currently declared by setting a process option:

```r
options(shiny.mcp.tools = list(
  list(
    name = "get_sample_stats",
    description = "Summary statistics for a normal sample of size n.",
    inputSchema = list(
      type = "object",
      properties = list(n = list(type = "integer")),
      required = list("n")
    ),
    handler = function(args) {
      x <- rnorm(args$n)
      list(n = args$n, mean = mean(x), sd = stats::sd(x))
    }
  )
))
```

Two problems:

1. **Hand-written JSON Schema.** Authors write nested `inputSchema` lists by
   hand — verbose and error-prone.
2. **Configuration via `options()`.** The adjacent R MCP ecosystem
   (`mcptools`, James Wade's `shinymcp`) has converged on `ellmer::tool()` as
   the tool-definition type and passes tools to a function, not an option.

This feature shipped as experimental in #4407 and has **not** reached `main`,
so the API can change with no deprecation burden.

## Scope

**In scope (this PR):** author tools only — replace
`options(shiny.mcp.tools = ...)` with a function that accepts
`ellmer::tool()` objects.

**Out of scope (another day):** the app-opening tool
(`options(shiny.mcp.tool = ...)`) and every other `shiny.mcp.*` option
(`shiny.mcp`, `appId`, `direct`, `displayModes`, `stdio`, `origin`) stay
exactly as they are.

## Prior art

The entire R MCP-authoring neighborhood already standardized on
`ellmer::tool()`:

| Package | Author API | Tool def |
|---|---|---|
| `mcptools` | `mcp_server(tools = list(...))` | `ellmer::tool()` |
| `shinymcp` | `mcp_app(ui, tools)` | `ellmer::tool()` |
| **this design** | `registerMcpTool(...)` | `ellmer::tool()` |

The MCP Apps SDK (`modelcontextprotocol/ext-apps`) is TypeScript and has no
cross-language tool-def abstraction — on the wire everything is plain JSON
Schema. `ellmer::tool()` is purely the R ergonomics layer above that wire
format, so adopting it does not diverge from MCP Apps.

The registration *verb* (`registerMcpTool()`) rather than a `tools=`
constructor arg is the deliberate Scope-A choice: the on/off and transport
knobs stay as options this PR, so there is no constructor to hang `tools=`
off yet.

## Public API

One new exported function:

```r
registerMcpTool(
  ellmer::tool(
    function(n) {
      x <- rnorm(n)
      list(n = n, mean = mean(x), sd = stats::sd(x))
    },
    name = "get_sample_stats",
    description = "Summary statistics for a normal sample of size n.",
    arguments = list(n = ellmer::type_integer("Sample size", required = TRUE))
  )
)
```

- Accepts one or more `ellmer::ToolDef` objects via `...`.
- Called at the top level of `app.R`, before the app starts — the same
  process-global, sessionless model author tools already use.
- Returns invisibly (`NULL`).

### Name rationale

Matches `registerInputHandler(type, fun)` — Shiny's existing string-keyed,
callable-holding registry — almost exactly (one callable per name). Leaves
room for a future `removeMcpTool(name)` if ever wanted. Chosen over the
`mcp`-prefixed `mcpTools()` because the `register*` precedent is the closer
structural analogue.

### Registration semantics

- Tools accumulate in `.globals$mcpAuthorTools`, a named list keyed by
  `tool@name`.
- **Last-write-wins by name** (not an error). Re-sourcing `app.R` or
  re-running `runApp()` during development re-registers cleanly. No `force`
  argument (YAGNI).

### Validation — eager, fail-fast

Because registration is now an explicit call rather than a lazily-read
option, each argument is validated *at registration time*:

- Must be an `ellmer::ToolDef` (checked with
  `inherits(x, "ellmer::ToolDef")`) → else `stop()`.
- `tool@name` must not collide with reserved names: the app-opening tool's
  name and the `_shiny_*` tunnel tool names → else `stop()`.

This **deletes** the old "skip invalid spec + warn from `tools/list`" code
path in `mcpAuthorTools()`, which only existed because options are read
lazily. `mcpAuthorTools()` becomes a plain reader of validated state; its
`warn` parameter is removed.

## Schema conversion

Reuse the `mcptools::tool_as_json()` pattern verbatim so the conversion
stays byte-aligned for future upstreaming:

```r
as_json <- getNamespace("ellmer")[["as_json"]]
inputSchema <- compact(as_json(ellmer::Provider("dummy", "dummy", "dummy"), tool@arguments))
inputSchema$description <- NULL
if (is.null(inputSchema$properties)) {
  inputSchema$properties <- structure(list(), names = character())
}
```

- The dummy `ellmer::Provider()` needs no API key; `as_json()` dispatches to
  the provider-agnostic base method for `Type` objects and emits standard
  JSON Schema.
- `ellmer::as_json` is not exported; access it via
  `getNamespace("ellmer")[["as_json"]]` exactly as `mcptools` does. Guard by
  pinning a minimum `ellmer` version in `Suggests` (the version providing the
  S7 `ToolDef`/`Provider`/`as_json` surface used here). Pin `>= 0.4.0` to
  match the version verified against during implementation; relax only if a
  lower bound is confirmed to expose the same surface.
- Tool `title` and `annotations` pass through into the `tools/list` entry if
  present (cheap fidelity, mirrors `tool_as_json`).

The resulting `tools/list` entry for an author tool is unchanged in shape
from today (`name`, `description`, `inputSchema`, optional `title`/
`annotations`) — only its *source* changes from a hand-written list to an
ellmer conversion.

## Handler invocation

An `ellmer::ToolDef` *is* the underlying function, so `mcpAuthorToolCall()`
changes its call convention:

```r
# before
tryCatch(tool$handler(args), error = function(e) e)
# after
tryCatch(rlang::exec(tool, !!!args), error = function(e) e)
```

Arguments are spread as named parameters directly, rather than passed as a
single `args` list. **Return-value handling is unchanged:**

- character / numeric / logical → text content
- list → `structuredContent` + JSON text
- promises → resolved via `hybrid_chain`
- errors → `isError` result

(`ellmer::ContentToolResult` return handling is out of scope; author tools
return plain values as today.)

## Dependency

`ellmer` moves to `Suggests`. `registerMcpTool()` calls
`rlang::check_installed("ellmer")` (rlang is already imported) with an
actionable message. Nothing in the core MCP request path touches ellmer
unless a tool has been registered — the conversion runs lazily from
`tools/list`, and if a tool is registered ellmer is by definition present.

## Files touched

- `R/mcp-server.R` — `mcpAuthorTools()` reads `.globals$mcpAuthorTools`
  (drop the `warn` path and the `getOption` read); new schema-conversion
  helper; `mcpAuthorToolCall()` uses `rlang::exec()`.
- **New `R/mcp-tools.R`** — `registerMcpTool()` + roxygen. Rewrite the
  "Additional tools" `@section` in `R/mcp-session.R` to point at it.
- `R/globals.R` (or wherever `.globals` fields are initialised) — default
  `mcpAuthorTools` to an empty named list if needed.
- `DESCRIPTION` — add `ellmer (>= 0.4.0)` to `Suggests`.
- `NAMESPACE` — export `registerMcpTool` (via roxygen / `devtools::document()`).
- `man/` — regenerate; add the new topic to
  `tools/documentation/pkgdown.yml` under the "MCP Apps" section.
- `NEWS.md` — note the author-tool API is `registerMcpTool()` +
  `ellmer::tool()` (fold into the #4407 entry).
- `mcp/demo-app`, `mcp/demo-app2` — migrate `options(shiny.mcp.tools = ...)`
  to `registerMcpTool()`.
- `mcp/architecture.md` and the `./mcp` mirrors — update the author-tools
  section (standing preference: keep repo-root `mcp/` docs current).

## Testing

All MCP author-tool tests gated with `skip_if_not_installed("ellmer")`.

- `tools/list` emits the correct JSON Schema for an `ellmer::tool()` with
  typed arguments (raw-JSON assertion, since R-side list parsing hides the
  named-list-serializes-as-object trap noted in project history).
- A `tools/call` against a registered `ellmer::tool()` invokes the function
  with spread named args and returns text / `structuredContent` correctly.
- Registering two tools with the same name → last wins.
- Registering a tool whose name is reserved (app tool, `_shiny_*`) → error.
- `registerMcpTool()` errors clearly when passed a non-`ToolDef`.
- (If feasible in CI) `registerMcpTool()` errors when ellmer is not
  installed — otherwise covered by the `check_installed` call path.

## Non-goals / explicit exclusions

- No `force` argument, no `removeMcpTool()` (can be added later).
- No `options(shiny.mcp.tools = ...)` fallback — the function is the only
  path.
- No changes to the app-opening tool or any other `shiny.mcp.*` option.
- No `ellmer::ContentToolResult` return-value handling.
