# Design: replace `shiny.mcp.*` options with `mcpConfigure()`

- **Date:** 2026-07-15
- **Branch:** `schloerke/thimphu` (targets `origin/mcp`)
- **Status:** implemented (Phase A + Phase B complete)
- **Scope:** one PR, implemented in two internal phases (A then B)

## Problem

MCP App support is configured entirely through base R `options(shiny.mcp.* = ...)`:

- `shiny.mcp` — enable MCP serving (gates startup in `server.R`)
- `shiny.mcp.stdio` — enable the stdio transport
- `shiny.mcp.appId` — app identity for gateway merging
- `shiny.mcp.direct` — direct-connect fast path toggle
- `shiny.mcp.displayModes` — allowed display modes
- `shiny.mcp.origin` — explicit deploy base URL
- `shiny.mcp.tool` — nested `list(name, description, inputSchema)` for the app-opening tool

`options()` is an unregulated, process-global namespace: no validation, no
discoverability, no autocomplete, silent typos, and values leak across apps in
the same R session. It gives us no single place to gate or validate how an app
opts into MCP behavior.

The feature is experimental and unreleased on this branch, so we can remove the
options outright — no deprecation shims.

## Goals

- Replace all seven `shiny.mcp.*` options with one validated, documented,
  autocomplete-friendly function: `mcpConfigure()`.
- Make opting in **explicit and safe**: an app is served over MCP *only if* it
  calls `mcpConfigure(enabled = TRUE)` (the default). An app that never calls
  `mcpConfigure()` is never exposed over MCP.
- Type-assert every argument at the single entry point (the "regulation" the
  loose `options()` surface lacked).
- Collapse the three-knob nested `tool` config: derive the tool name from
  `appId`, and express its declared arguments with `ellmer` types (matching
  `registerMcpTool()`), not ad-hoc lists.
- Eliminate duplicated author code for "set initial state from the model" vs
  "restore a bookmark" by routing the model's opening arguments through Shiny's
  existing `RestoreContext`.

## Non-goals

- No deprecation path for `shiny.mcp.*` (experimental, unreleased).
- No change to the `sendCustomMessage` channels (`shiny.mcp.updateModelContext`,
  `shiny.mcp.sendMessage`, `shiny.mcp.requestDisplayMode`) — those are not
  options.
- No change to `registerMcpTool()`'s public surface (it already uses `.globals`
  + `ellmer::tool()`), beyond a `@seealso` doc update.

## Public API — `mcpConfigure()`

```r
mcpConfigure(
  appId        = NULL,                              # unique id for gateway merging
  description  = NULL,                              # what the model reads to decide whether to open the app
  arguments    = NULL,                              # named list of ellmer type_*() objects (the whitelist)
  direct       = TRUE,                              # direct-connect fast path
  displayModes = c("inline", "fullscreen", "pip"),  # allowed display modes
  origin       = NULL,                              # explicit deploy base URL
  stdio        = FALSE,                             # enable stdio transport
  enabled      = TRUE                               # LAST: gates whether MCP is served at all
)
```

Called at the top level of `app.R` / `global.R` (or before `runApp()`), like
`registerMcpTool()`. Works uniformly for single-file, `app.R`, and two-file
(`ui.R`/`server.R`) apps because it does not depend on the `shinyApp()` call.

### `enabled` is the opt-in safeguard

- Default `TRUE`: a call to `mcpConfigure()` turns MCP on.
- `enabled = FALSE`: configure-but-disable (rare; e.g. staging a config).
- **No call to `mcpConfigure()` at all ⇒ MCP is not enabled.** This is the
  central safety property: apps that were never meant to be MCP-exposed cannot
  be, regardless of ambient `options()` in the R session.

`enabled` is placed last so typical calls read naturally by the meaningful
config first: `mcpConfigure("demo", "Open the sales dashboard.")`.

### Validation / type assertions

Every argument is asserted at entry, in the repo's existing MCP style
(`stop(..., call. = FALSE)`; no checkmate/cli dependency):

- `appId`: `NULL`, or a single non-`NA` character string matching
  `^[A-Za-z0-9_-]+$`. **Invalid ⇒ error** (today it silently warns-and-ignores;
  we tighten to an error).
- `description`: `NULL` or a single character string.
- `arguments`: `NULL`, or a **named** list in which every element inherits from
  the `ellmer` type class (as produced by `ellmer::type_*()`). Any non-type
  element ⇒ error. Requires `ellmer`; assert with
  `rlang::check_installed("ellmer", ...)` when `arguments` is non-`NULL`.
- `direct`, `stdio`, `enabled`: single non-`NA` logicals.
- `displayModes`: character vector; intersected with
  `c("inline","fullscreen","pip")`; empty result falls back to `"inline"`
  (preserving current `mcpDisplayModes()` behavior).
- `origin`: `NULL` or a single character string.

### Tool naming derived from `appId`

Drops the manual `tool$name` knob. The single app-opening tool's exposed name
is derived from `appId`:

- `appId = NULL` ⇒ `open_shiny_app`
- `appId = "cars"` ⇒ `open_cars_app`

This matches the gateway README's existing `open_shiny_app` / `open_cars_app`
pattern, and because `appId` is already required to be unique per app for
gateway merging, tool-name uniqueness across a merged gateway becomes automatic
— the author no longer maintains a separate unique tool name.

### `arguments` — the model-settable whitelist

`arguments` is a named list of `ellmer` type objects, mirroring the `arguments`
parameter of `ellmer::tool()`:

```r
mcpConfigure(
  appId = "demo",
  description = "Open the histogram demo.",
  arguments = list(
    n    = ellmer::type_integer("Number of bins (10-500)"),
    note = ellmer::type_string("A note to show in the app")
  )
)
```

It serves two purposes, both bounded by this whitelist:

1. **Declaration to the model.** Converted to JSON Schema via the same `ellmer`
   `as_json()` path `mcpToolInputSchema()` already uses, and published as the
   tool's `inputSchema` in `tools/list`. Without it, the model has no reason to
   send arguments.
2. **Whitelist filter.** Any argument the model sends (at open or via an
   update) is filtered down to the declared names before it reaches the app.
   Undeclared keys are dropped. (Bookmark/restore state may hold arbitrarily
   more; MCP-sourced values are constrained to the whitelist.)

We deliberately do **not** accept a full `ellmer::tool()` object here: the
app-opening tool has no R-side handler (opening the app *is* the effect; values
flow to the client), so `description` + `arguments` is the honest shape. Full
model-callable R functions remain the job of `registerMcpTool(ellmer::tool())`.

## Internal storage

`.globals$mcp` holds the validated, defaulted config as a plain list, matching
the `registerMcpTool()` → `.globals$mcpAuthorTools` precedent. `mcpConfigure()`
validates its arguments and writes the resulting config into `.globals$mcp`.

Every current `getOption("shiny.mcp.*")` read is rewritten to read
`.globals$mcp`:

| Accessor (unchanged name)         | Was                                   | Becomes                          |
|-----------------------------------|---------------------------------------|----------------------------------|
| `mcpEnabled()`                    | `getOption("shiny.mcp")`              | `isTRUE(.globals$mcp$enabled)`   |
| `mcpStdioEnabled()`               | `getOption("shiny.mcp.stdio")`        | `.globals$mcp$stdio`             |
| `mcpAppId()`                      | `getOption("shiny.mcp.appId")`        | `.globals$mcp$appId`             |
| `mcpDirectEnabled()`              | `getOption("shiny.mcp.direct")`       | `.globals$mcp$direct`            |
| `mcpDisplayModes()`               | `getOption("shiny.mcp.displayModes")` | `.globals$mcp$displayModes`      |
| `mcpToolInfo()`                   | `getOption("shiny.mcp.tool")`         | `.globals$mcp$description` + name derived from `appId` + `arguments`→schema |
| `mcpDirectBase()` origin override | `getOption("shiny.mcp.origin")`       | `.globals$mcp$origin`            |

Because validation and defaulting happen in `mcpConfigure()`, the accessors
simplify to field reads. Timing is unchanged: `mcpConfigure()` is called before
the app starts, and the accessors are read at the same points as today
(`server.R` startup gates, `tools/list`, `resources/read`, etc.).

`mcpAppId()`'s current bad-id warn-and-ignore branch and `.globals$mcpAppIdWarned`
flag are removed — invalid ids now error in `mcpConfigure()` instead.

### Reset semantics

Since config lives in process-global `.globals` (like `mcpAuthorTools`), it
persists for the R session. `mcpConfigure()` replaces the full config on each
call (validate → overwrite). This is acceptable for the normal one-app-per-
process MCP workflow and matches the existing `.globals$mcpAuthorTools`
behavior. (A future refinement could capture/clear per-app like
`captureAppOptions()`; out of scope here.)

## Argument delivery: init vs. update

The model can supply arguments at two moments, which map to two distinct Shiny
mechanisms. The author's touchpoints differ accordingly.

### Init — via `RestoreContext` (anti-flash)

When the model opens the app (`open_<appId>_app(n = 200)`), the whitelisted
arguments are injected into the session's **`RestoreContext`** (see
`R/bookmark-state.R`). This reuses Shiny's existing restoration machinery:

- Standard inputs auto-restore via `restoreInput(id, default)` (every input
  widget calls it at construction). A slider `n` is *constructed* at 200 — no
  observer, no visible jump from a default to the model's value (**this is why
  we use RestoreContext: to avoid flashing stale state**).
- Non-input state is available through `onRestore(function(state) ...)` /
  `onRestored(...)`, exactly as bookmarking already works.

An app that is already bookmarkable reuses its restore logic verbatim for MCP
opens. Author writes nothing new for the init path of input-backed args.

**Implementation approach (the trickiest part; to be detailed in the plan):**
the MCP bridge must make the opening arguments available at *session start*,
before the server function runs, so `RestoreContext` can consume them. The
intended approach is to encode the whitelisted opening args into the session's
restore context by reusing Shiny's bookmark URL-state encoding on the (tunneled
or direct) websocket connection, then let the normal `RestoreContext` parse
them. Key risk to resolve in the plan: ordering — the initial tool input from
the host's `ui/initialize` handshake must be captured before `Shiny`'s socket
start triggers session creation (the bridge already defers `socket.start()` via
`setTimeout(..., 0)`; `srcts/src/mcp/index.ts`).

### After-init — via `mcpUpdates()` (reactive)

When the model changes an argument on the already-open app
(`open_<appId>_app(n = 50)` again, routed by the host to the live iframe), there
is no bookmark analog — restoration is a session-start-only concept. These
post-init values arrive on the existing client-data channel
(`.clientdata_mcp_tool_input`, set by the bridge's `ontoolinput`) and are read
reactively.

`mcpToolInput()` is **renamed to `mcpUpdates()`** to reflect this: after Phase B,
init args flow through `RestoreContext`, so the reactive channel only ever
carries *post-init updates*. The author wires them with the normal updaters:

```r
observe({
  n <- mcpUpdates()$n
  if (!is.null(n)) updateSliderInput(session, "n", value = n)
})
```

### Known tradeoff: non-input state is touched in two places

For input-backed args (`n`), init is free (auto-restore) and only the after-init
updater is authored. For **non-input display state** (`note` rendered in a
header), the author writes `onRestore()` for init *and* an `mcpUpdates()`
observer for after-init. These two touchpoints are inherent — bookmarking has no
live-update concept — but the author can factor the shared application logic
into one helper called from both. This is an accepted cost of keeping init
flash-free via `RestoreContext` while still supporting live re-steering.

## Removal / migration

- Delete all `getOption("shiny.mcp.*")` reads.
- `R/mcp-session.R`: rewrite roxygen (the `mcp-session` topic and examples) to
  use `mcpConfigure()`; rename `mcpToolInput()` → `mcpUpdates()` (function,
  roxygen `@rdname`, `@return`, examples). Update the `displayModes` docs to
  reference `mcpConfigure(displayModes = ...)`.
- `R/mcp-tools.R`: update the `registerMcpTool()` example header
  (`options(shiny.mcp = TRUE)` → `mcpConfigure()`) and `@seealso [mcpToolInput()]`
  → `[mcpUpdates()]`.
- `R/mcp-server.R`, `R/mcp-app.R`, `R/mcp-stdio.R`: rewrite the option-reading
  accessors and the file-header comments that document `options(shiny.mcp*)`.
- `NAMESPACE` / roxygen: export `mcpConfigure`; export `mcpUpdates`; remove
  `mcpToolInput` export.
- `mcp/**` docs (`README.md`, `architecture.md`, `design-spec.md`,
  `gateway/README.md`, `gateway/shiny-mcp-gateway.mjs` comments, etc.) and both
  `mcp/demo-app*/app.R`: convert every `options(shiny.mcp*)` usage to
  `mcpConfigure()`. Keep repo-root `mcp/` docs current (per project convention).
- `man/`: regenerate with `devtools::document()`.

## Files touched

- `R/mcp-config.R` *(new)* — `mcpConfigure()`, its validators, and the
  `.globals$mcp` accessors (or the accessors stay in `mcp-server.R`; decide in
  the plan).
- `R/globals.R` — initialize `.globals$mcp` (e.g. `NULL` / empty list).
- `R/mcp-server.R`, `R/mcp-app.R`, `R/mcp-stdio.R` — accessor rewrites + header
  comments.
- `R/mcp-session.R`, `R/mcp-tools.R` — rename + docs.
- `R/bookmark-state.R` and `srcts/src/mcp/index.ts` (+ rebuilt
  `inst/www/shared/shiny-mcp-bridge.js`) — init-args-via-RestoreContext (Phase B).
- `tests/testthat/test-mcp-*.R` — see below.
- `mcp/**`, `man/`, `NAMESPACE`, `NEWS.md`.

## Testing

- Rewrite `test-mcp-{app,server,session,stdio}.R` to drive configuration through
  `mcpConfigure()` instead of `options(shiny.mcp*)`.
- Validation tests: invalid `appId` errors; non-`ellmer`-type `arguments`
  errors; unnamed `arguments` errors; `displayModes` intersection/fallback;
  non-scalar/`NA` logicals error.
- Opt-in safeguard test: no `mcpConfigure()` call ⇒ `mcpEnabled()` is `FALSE` and
  no `/mcp` endpoint is mounted; `mcpConfigure(enabled = FALSE)` ⇒ disabled.
- Tool-naming test: `appId` derives `open_<appId>_app`; default `open_shiny_app`.
- `arguments` → `inputSchema` conversion test (reuse the `mcpToolInputSchema`
  path).
- Phase B: init args injected into `RestoreContext` restore inputs / surface in
  `onRestore`; post-init values surface via `mcpUpdates()`; whitelist filtering
  drops undeclared keys.

## Implementation phasing (single PR)

- **Phase A — config API refactor (no runtime behavior change).**
  `mcpConfigure()` + `.globals$mcp` + accessor rewrites + option removal +
  `mcpToolInput()` → `mcpUpdates()` rename + docs/tests. Arguments still surface
  via the reactive channel exactly as `mcpToolInput()` did, so behavior is
  unchanged; only the configuration surface and the reactive's name change.
- **Phase B — init via `RestoreContext` (IMPLEMENTED).** Opening arguments are
  routed through the restore context (R + JS bridge), so init is flash-free and
  `mcpUpdates()` is post-init-only. Server-side allow-list filtering
  (`mcpFilterArguments`, `mcpFilterRestore`) ensures only declared argument names
  reach the app on both the init and post-init paths.
