# mcpConfigure() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the seven `options(shiny.mcp.*)` options with one validated `mcpConfigure()` function backed by `.globals$mcp`, rename `mcpToolInput()` → `mcpUpdates()`, and route the model's app-opening arguments through Shiny's `RestoreContext` so init is flash-free.

**Architecture:** A single validated entry point (`mcpConfigure()`) writes a defaulted config list into `.globals$mcp`; all internal accessors read from there instead of `getOption()`. Opting in is explicit — no `mcpConfigure()` call means MCP is never served. The model's opening arguments (init) flow through the existing bookmark `RestoreContext` for flash-free input restoration; post-init argument updates flow through the reactive `mcpUpdates()` channel.

**Tech Stack:** R (S7 via `ellmer`, R6 for `RestoreContext`), roxygen2, testthat 3e, TypeScript (`srcts/src/mcp/`, bundled to `inst/www/shared/shiny-mcp-bridge.js`).

## Global Constraints

- The MCP feature is **experimental and unreleased** on this branch — remove `shiny.mcp.*` outright, with **no deprecation shims**.
- Validation uses base `stop(..., call. = FALSE)` — the repo's MCP-code convention. Do **not** add `checkmate` or `cli` dependencies.
- `ellmer` is a Suggests dependency: guard its use with `rlang::check_installed("ellmer", ...)`, matching `registerMcpTool()` (`R/mcp-tools.R:45`).
- Tool name derives from `appId`: `NULL` → `open_shiny_app`; `"cars"` → `open_cars_app`.
- `appId` must match `^[A-Za-z0-9_-]+$` or it is an **error** (was warn-and-ignore).
- Keep repo-root `mcp/` docs/demos current (project convention).
- Every `.clientdata_*` value the JS bridge sets goes through `Shiny.setInputValue`.

---

## File Structure

- **Create `R/mcp-config.R`** — `mcpConfigure()`, its validators, the `.globals$mcp` field accessors that replace `getOption()`, tool-name derivation, and the `arguments`→JSON-Schema converter. One responsibility: MCP configuration surface + storage.
- **Modify `R/globals.R`** — initialize `.globals$mcp <- NULL`.
- **Modify `R/mcp-server.R`** — accessors (`mcpEnabled`, `mcpAppId`, `mcpDirectEnabled`, `mcpDisplayModes`, `mcpToolInfo`, origin read in `mcpDirectBase`) either move to `mcp-config.R` or have their bodies rewritten; delete the `appId` warn branch. File-header comment updated.
- **Modify `R/mcp-stdio.R`** — `mcpStdioEnabled()` body + header comment.
- **Modify `R/mcp-session.R`** — rename `mcpToolInput()` → `mcpUpdates()`; rewrite roxygen/examples to `mcpConfigure()`.
- **Modify `R/mcp-tools.R`** — example header + `@seealso` doc.
- **Modify `R/shiny.R`** — extract `createRestoreObservers()` out of `createBookmarkObservers()` (Phase B).
- **Modify `R/server.R`** — build an active `RestoreContext` from MCP opening args for MCP sessions (Phase B).
- **Modify `srcts/src/mcp/index.ts`** (+ rebuilt bundle) — encode initial args into restore clientdata; route later updates to `mcpUpdates` (Phase B).
- **Modify tests** — `tests/testthat/helper-mcp.R` (new), `test-mcp-{app,server,session,stdio}.R`.
- **Docs** — `mcp/**`, `mcp/demo-app*/app.R`, `NAMESPACE`, `man/`, `NEWS.md`.

---

# PHASE A — Config API refactor (no runtime behavior change)

### Task A1: `mcpConfigure()` + validators + storage

**Files:**
- Create: `R/mcp-config.R`
- Modify: `R/globals.R:3` (add `.globals$mcp <- NULL`)
- Test: `tests/testthat/test-mcp-config.R` (new)

**Interfaces:**
- Produces:
  - `mcpConfigure(appId = NULL, description = NULL, arguments = NULL, direct = TRUE, displayModes = c("inline","fullscreen","pip"), origin = NULL, stdio = FALSE, enabled = TRUE)` → invisibly the stored config list; writes `.globals$mcp`.
  - `mcpToolName()` → `character(1)` derived from `.globals$mcp$appId`.
  - `mcpArgumentsSchema(arguments)` → JSON-Schema `list`.

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-mcp-config.R
test_that("mcpConfigure() stores a defaulted, enabled config", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure()
  expect_true(mcpEnabled())
  expect_equal(.globals$mcp$displayModes, c("inline", "fullscreen", "pip"))
  expect_true(.globals$mcp$direct)
  expect_false(.globals$mcp$stdio)
})

test_that("no mcpConfigure() call means MCP is disabled", {
  withr::defer(.globals$mcp <- NULL)
  .globals$mcp <- NULL
  expect_false(mcpEnabled())
})

test_that("mcpConfigure(enabled = FALSE) configures but disables", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(appId = "demo", enabled = FALSE)
  expect_false(mcpEnabled())
  expect_equal(.globals$mcp$appId, "demo")
})

test_that("invalid appId errors", {
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(appId = "not ok!"), "appId")
  expect_error(mcpConfigure(appId = c("a", "b")), "appId")
})

test_that("displayModes are intersected and fall back to inline", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(displayModes = c("fullscreen", "bogus"))
  expect_equal(.globals$mcp$displayModes, "fullscreen")
  mcpConfigure(displayModes = "bogus")
  expect_equal(.globals$mcp$displayModes, "inline")
})

test_that("scalar-logical and string args are asserted", {
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(direct = "yes"), "direct")
  expect_error(mcpConfigure(enabled = NA), "enabled")
  expect_error(mcpConfigure(description = c("a", "b")), "description")
})

test_that("arguments must be a named list of ellmer types", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(arguments = list(ellmer::type_integer("x"))), "named")
  expect_error(mcpConfigure(arguments = list(n = 1L)), "ellmer")
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  expect_named(.globals$mcp$arguments, "n")
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-config.R")'`
Expected: FAIL — `could not find function "mcpConfigure"`.

- [ ] **Step 3: Implement `R/mcp-config.R`**

```r
# MCP App configuration. Replaces the former options(shiny.mcp.*): a single
# validated entry point that stores a defaulted config list in .globals$mcp.
# An app is served over MCP only if it calls mcpConfigure(enabled = TRUE)
# (the default); with no call at all, MCP is never enabled.

MCP_DISPLAY_MODES <- c("inline", "fullscreen", "pip")

#' Configure a Shiny app as an MCP App
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Call `mcpConfigure()` at the top level of your app (in `app.R`, `global.R`,
#' or before [runApp()]) to serve it as an MCP App (SEP-1865). An app is
#' exposed over MCP **only** when it calls `mcpConfigure()` with
#' `enabled = TRUE` (the default); an app that never calls `mcpConfigure()` is
#' never served over MCP.
#'
#' @param appId Optional unique identifier (letters, digits, `_`, `-`) so a
#'   gateway can merge several Shiny MCP endpoints. Derives the app-opening
#'   tool name (`open_<appId>_app`) and the UI resource URI
#'   (`ui://shiny/<appId>`).
#' @param description Human/model-readable description of the app-opening tool;
#'   the model reads it to decide whether to open the app.
#' @param arguments Optional named list of \pkg{ellmer} type objects (e.g.
#'   [ellmer::type_integer()]) declaring the arguments the model may pass when
#'   opening the app. Published as the tool's input schema and used as an
#'   allow-list. Read within the app with [mcpUpdates()] (post-init) or via
#'   input/bookmark restoration (init).
#' @param direct Whether to advertise the direct-connect fast path (default
#'   `TRUE`).
#' @param displayModes Display modes the app supports; a subset of
#'   `c("inline", "fullscreen", "pip")`.
#' @param origin Optional explicit external base URL for the direct-connect
#'   fast path (may include a path).
#' @param stdio Whether to also speak MCP over stdin/stdout (default `FALSE`;
#'   not supported on Windows).
#' @param enabled Whether MCP serving is enabled (default `TRUE`). Placed last
#'   so typical calls lead with the meaningful config.
#'
#' @return Invisibly, the stored configuration list.
#' @seealso [mcpUpdates()] and the other `mcp*` session helpers;
#'   [registerMcpTool()] for model-callable tools.
#' @export
mcpConfigure <- function(
  appId = NULL,
  description = NULL,
  arguments = NULL,
  direct = TRUE,
  displayModes = c("inline", "fullscreen", "pip"),
  origin = NULL,
  stdio = FALSE,
  enabled = TRUE
) {
  assertMcpFlag(enabled, "enabled")
  assertMcpFlag(direct, "direct")
  assertMcpFlag(stdio, "stdio")
  assertMcpOptionalString(description, "description")
  assertMcpOptionalString(origin, "origin")
  appId <- assertMcpAppId(appId)
  arguments <- assertMcpArguments(arguments)

  modes <- intersect(as.character(displayModes), MCP_DISPLAY_MODES)
  if (length(modes) == 0) modes <- "inline"

  .globals$mcp <- list(
    enabled = enabled,
    appId = appId,
    description = description,
    arguments = arguments,
    direct = direct,
    displayModes = modes,
    origin = origin,
    stdio = stdio
  )
  invisible(.globals$mcp)
}

assertMcpFlag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be a single TRUE or FALSE.", call. = FALSE)
  }
}

assertMcpOptionalString <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be NULL or a single string.", call. = FALSE)
  }
}

assertMcpAppId <- function(appId) {
  if (is.null(appId)) return(NULL)
  if (!is.character(appId) || length(appId) != 1 || is.na(appId) || !nzchar(appId)) {
    stop("`appId` must be NULL or a single non-empty string.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", appId)) {
    stop(
      "`appId` may only contain letters, digits, '_' and '-'.",
      call. = FALSE
    )
  }
  appId
}

assertMcpArguments <- function(arguments) {
  if (is.null(arguments)) return(NULL)
  if (!is.list(arguments) || length(arguments) == 0) {
    stop("`arguments` must be NULL or a non-empty named list.", call. = FALSE)
  }
  nms <- names(arguments)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("`arguments` must be a named list.", call. = FALSE)
  }
  rlang::check_installed("ellmer", "to declare MCP tool `arguments`.")
  for (el in arguments) {
    if (!inherits(el, "ellmer::Type")) {
      stop(
        "Each element of `arguments` must be an ellmer type ",
        "(e.g. `ellmer::type_integer()`); received an object of class '",
        class(el)[1], "'.",
        call. = FALSE
      )
    }
  }
  arguments
}

# open_shiny_app, or open_<appId>_app when an appId is configured.
mcpToolName <- function() {
  id <- .globals$mcp$appId
  if (is.null(id)) "open_shiny_app" else paste0("open_", id, "_app")
}

# Convert the configured `arguments` (named list of ellmer types) into the
# JSON Schema published as the tool's inputSchema. Mirrors
# mcpToolInputSchema() (R/mcp-server.R) so the two conversions stay aligned.
mcpArgumentsSchema <- function(arguments) {
  if (is.null(arguments) || length(arguments) == 0) {
    return(list(type = "object", properties = empty_named_list()))
  }
  obj <- rlang::exec(ellmer::type_object, !!!arguments)
  as_json <- getNamespace("ellmer")[["as_json"]]
  schema <- as_json(ellmer::Provider("dummy", "dummy", "dummy"), obj)
  schema$description <- NULL
  if (is.null(schema$properties)) {
    schema$properties <- empty_named_list()
  }
  dropNulls(schema)
}
```

Add to `R/globals.R` after line 3 (`.globals$mcpAuthorTools <- list()`):

```r
.globals$mcp <- NULL
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-config.R")'`
Expected: PASS (all).

- [ ] **Step 5: Commit**

```bash
git add R/mcp-config.R R/globals.R tests/testthat/test-mcp-config.R
git commit -m "feat(mcp): add mcpConfigure() with validation and .globals\$mcp storage"
```

---

### Task A2: Accessors read `.globals$mcp` instead of `getOption()`

**Files:**
- Modify: `R/mcp-server.R:15-17,24-26,28-33,40-56,220-231` and `:106-109`
- Modify: `R/mcp-stdio.R:19-21`
- Test: `tests/testthat/test-mcp-config.R` (append)

**Interfaces:**
- Consumes: `.globals$mcp`, `mcpToolName()`, `mcpArgumentsSchema()` (Task A1).
- Produces: unchanged accessor names/return types — `mcpEnabled()`, `mcpStdioEnabled()`, `mcpAppId()`, `mcpDirectEnabled()`, `mcpDisplayModes()`, `mcpToolInfo()`.

- [ ] **Step 1: Write failing tests (append to test-mcp-config.R)**

```r
test_that("accessors read the stored config", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(
    appId = "cars", description = "Show the cars app.",
    direct = FALSE, displayModes = "inline", origin = "https://x.example/app",
    stdio = TRUE
  )
  expect_true(mcpEnabled())
  expect_true(mcpStdioEnabled())
  expect_equal(mcpAppId(), "cars")
  expect_false(mcpDirectEnabled())
  expect_equal(mcpDisplayModes(), "inline")
  info <- mcpToolInfo()
  expect_equal(info$name, "open_cars_app")
  expect_equal(info$description, "Show the cars app.")
})

test_that("mcpToolInfo() falls back to defaults and builds schema", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  info <- mcpToolInfo()
  expect_equal(info$name, "open_shiny_app")
  expect_match(info$description, "interactive Shiny application")
  expect_equal(info$inputSchema$type, "object")
  expect_true("n" %in% names(info$inputSchema$properties))
})
```

- [ ] **Step 2: Run to verify failure**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-config.R")'`
Expected: FAIL — accessors still read `getOption()`, so `mcpAppId()`/`mcpToolInfo()` return old values.

- [ ] **Step 3: Rewrite accessor bodies**

In `R/mcp-server.R`, replace these functions:

```r
mcpEnabled <- function() {
  isTRUE(.globals$mcp$enabled)
}

mcpDirectEnabled <- function() {
  isTRUE(.globals$mcp$direct)
}

mcpDisplayModes <- function() {
  .globals$mcp$displayModes %||% "inline"
}

# Optional app identity (validated in mcpConfigure()).
mcpAppId <- function() {
  .globals$mcp$appId
}
```

Delete the old `.globals$mcpAppIdWarned` branch entirely (the warn-and-ignore is gone; invalid ids now error in `mcpConfigure()`).

Rewrite `mcpToolInfo()`:

```r
mcpToolInfo <- function() {
  list(
    name = mcpToolName(),
    description = .globals$mcp$description %||% paste(
      "Open the interactive Shiny application so the user can view and",
      "interact with it. Call this when the user wants to see the app."
    ),
    inputSchema = mcpArgumentsSchema(.globals$mcp$arguments)
  )
}
```

In `mcpDirectBase()` (`R/mcp-server.R:106`), replace:

```r
  origin_opt <- .globals$mcp$origin
```

In `R/mcp-stdio.R`, replace `mcpStdioEnabled()`:

```r
mcpStdioEnabled <- function() {
  isTRUE(.globals$mcp$stdio)
}
```

- [ ] **Step 4: Run to verify pass**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-config.R")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/mcp-server.R R/mcp-stdio.R tests/testthat/test-mcp-config.R
git commit -m "refactor(mcp): read config from .globals\$mcp instead of options()"
```

---

### Task A3: Rename `mcpToolInput()` → `mcpUpdates()`

**Files:**
- Modify: `R/mcp-session.R:131-138` (function) and `:1-106` (roxygen)
- Modify: `R/mcp-tools.R:42`
- Modify: `NAMESPACE` (regenerated in Task A5)
- Test: `tests/testthat/test-mcp-session.R`

**Interfaces:**
- Produces: `mcpUpdates(session = getDefaultReactiveDomain())` → named list or `NULL`. `mcpToolInput` no longer exists/exported.

- [ ] **Step 1: Update the failing test first**

In `tests/testthat/test-mcp-session.R`, rename any `mcpToolInput(` call to `mcpUpdates(`. (Search the file; there is coverage around the `mcp_tool_input` clientData read.)

- [ ] **Step 2: Rename the function**

In `R/mcp-session.R:131-138`:

```r
#' @rdname mcp-session
#' @export
mcpUpdates <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("mcpUpdates() must be called from within a Shiny session")
  }
  mcpParseClientData(session$clientData$mcp_tool_input)
}
```

Update the roxygen bullet at `R/mcp-session.R:13-16`:

```r
#' * `mcpUpdates()` returns arguments the model supplies *after* the app is
#'   open (a reactive read; initial arguments are applied via input/bookmark
#'   restoration — see `mcpConfigure(arguments = )`). Declare the allowed
#'   arguments with `mcpConfigure(arguments = list(...))`.
```

Update `@return` (`R/mcp-session.R:66-68`): change `mcpToolInput()` → `mcpUpdates()`.

In `R/mcp-tools.R:42`, change `@seealso [mcpToolInput()]` → `@seealso [mcpUpdates()]`.

- [ ] **Step 3: Run tests**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-session.R")'`
Expected: PASS.

- [ ] **Step 4: Verify no stale references remain**

Run: `grep -rn "mcpToolInput" R/ tests/ mcp/`
Expected: no matches (docs handled in Task A5; if `mcp/` still references it, fix in A5).

- [ ] **Step 5: Commit**

```bash
git add R/mcp-session.R R/mcp-tools.R tests/testthat/test-mcp-session.R
git commit -m "refactor(mcp): rename mcpToolInput() to mcpUpdates()"
```

---

### Task A4: Test helper + convert tests off `options(shiny.mcp*)`

**Files:**
- Create: `tests/testthat/helper-mcp.R`
- Modify: `tests/testthat/test-mcp-app.R`, `test-mcp-server.R`, `test-mcp-session.R`, `test-mcp-stdio.R`

**Interfaces:**
- Produces: `local_mcp_config(..., .env = parent.frame())` — sets `.globals$mcp` via `mcpConfigure(...)` and restores the previous value on exit.

- [ ] **Step 1: Write the helper**

```r
# tests/testthat/helper-mcp.R
# Set MCP config for the duration of the calling test, restoring the prior
# .globals$mcp afterwards. Replaces the old withr::with_options(shiny.mcp*).
local_mcp_config <- function(..., .env = parent.frame()) {
  old <- .globals$mcp
  withr::defer(.globals$mcp <- old, envir = .env)
  mcpConfigure(...)
}
```

- [ ] **Step 2: Convert each call site**

Convert every `withr::with_options(list(shiny.mcp... ), { ... })` and
`withr::local_options(list(shiny.mcp... ))` to `local_mcp_config(...)`.
Mapping of option → argument:

| Old option | New `mcpConfigure()` arg |
|---|---|
| `shiny.mcp = TRUE` | (implicit — any `local_mcp_config()` sets `enabled = TRUE`) |
| `shiny.mcp = FALSE` | `local_mcp_config(enabled = FALSE)` |
| `shiny.mcp.stdio = TRUE` | `stdio = TRUE` |
| `shiny.mcp.direct = FALSE` | `direct = FALSE` |
| `shiny.mcp.appId = "sales"` | `appId = "sales"` |
| `shiny.mcp.origin = "…"` | `origin = "…"` |
| `shiny.mcp.tool = list(name=…, description=…)` | `description = …` (drop `name`; assert derived `open_<appId>_app` instead) |
| `shiny.mcp.tool = list(inputSchema = schema)` | `arguments = list(...)` (ellmer types) |

Representative conversion (`test-mcp-app.R:7`):

```r
# before
withr::with_options(list(shiny.mcp = TRUE), {
  # ...
})
# after
local_mcp_config()
# ... (de-indent the block; withr::defer cleans up at test end)
```

Exact call sites to convert (from `grep -n "shiny.mcp" tests/testthat/`):
- `test-mcp-app.R`: lines 7, 175, 203, 242 (`shiny.mcp`, `shiny.mcp.direct`).
- `test-mcp-server.R`: 67, 89, 95, 220, 225-226, 311-312 (`tool`, `enabled`, `origin`, `appId`).
- `test-mcp-session.R`: 149 (`shiny.mcp.tool = list(inputSchema = schema)` → `arguments`).
- `test-mcp-stdio.R`: 96 is inside a **subprocess Rscript string** — rewrite that string from `options(shiny.mcp = TRUE, shiny.mcp.stdio = TRUE)` to `shiny::mcpConfigure(stdio = TRUE)`.

Note: `test-mcp-server.R:244-248` (`invalid shiny.mcp.appId is ignored` / `expect_warning`) is now invalid behavior — **replace** that test with an error expectation (already covered in `test-mcp-config.R`); delete it here.

`test-mcp-session.R:127,130,174` reference `shiny.mcp.updateModelContext` / `sendMessage` / `requestDisplayMode` — these are **sendCustomMessage channel names, not options**. Leave them unchanged.

- [ ] **Step 3: Run the full MCP suite**

Run: `R -q -e 'devtools::load_all(); testthat::test_dir("tests/testthat", filter = "mcp")'`
Expected: PASS (all mcp test files).

- [ ] **Step 4: Confirm no option references remain in tests**

Run: `grep -rn "shiny\.mcp\.\(tool\|appId\|direct\|origin\|stdio\|displayModes\)\|list(shiny.mcp" tests/`
Expected: no matches (the three sendCustomMessage names are fine).

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/helper-mcp.R tests/testthat/test-mcp-*.R
git commit -m "test(mcp): drive config via mcpConfigure() instead of options()"
```

---

### Task A5: Docs, NAMESPACE, demos, NEWS

**Files:**
- Modify: `R/mcp-session.R`, `R/mcp-server.R:1-9`, `R/mcp-app.R:1-9`, `R/mcp-stdio.R:1-18` (header comments)
- Modify: `mcp/**` (`README.md`, `architecture.md`, `design-spec.md`, `gateway/README.md`, `gateway/shiny-mcp-gateway.mjs`, `implementation-plan-*.md`), `mcp/demo-app/app.R`, `mcp/demo-app2/app.R`
- Regenerate: `NAMESPACE`, `man/`
- Modify: `NEWS.md`

- [ ] **Step 1: Convert every `options(shiny.mcp*)` in prose/comments/demos**

Run to find them all: `grep -rln "shiny.mcp" R/ mcp/ NEWS.md`

For each, replace the option form with the `mcpConfigure()` form. Examples:
- `options(shiny.mcp = TRUE)` → `mcpConfigure()`
- `options(shiny.mcp.appId = "demo")` → fold into `mcpConfigure(appId = "demo")`
- `options(shiny.mcp.tool = list(name=…, description=…, inputSchema=…))` → `mcpConfigure(description = …, arguments = list(...))`
- `options(shiny.mcp.direct = FALSE)` → `mcpConfigure(direct = FALSE)`
- `options(shiny.mcp.displayModes = c("inline","fullscreen"))` → `mcpConfigure(displayModes = c("inline","fullscreen"))`

In `mcp/demo-app/app.R`, collapse the three `options()` calls into one:

```r
library(shiny)

mcpConfigure(
  appId = "demo",
  description = paste(
    "Open the interactive demo dashboard. Optionally pass `note` (shown in",
    "the app) and `n` (initial number of observations, 10-500)."
  ),
  arguments = list(
    note = ellmer::type_string("A note to show in the app"),
    n    = ellmer::type_integer("Initial observations (10-500)")
  )
)
```

Update the `mcp/gateway/*` comment lines that say `options(shiny.mcp.appId/tool)` to `mcpConfigure(appId = , description = )`.

- [ ] **Step 2: Rewrite the `mcp-session` roxygen intro + examples**

In `R/mcp-session.R:5-9` and the `@examples` block (`:72-103`), replace the `options(shiny.mcp = TRUE)` / `options(shiny.mcp.tool = ...)` usage with `mcpConfigure(...)`, and any `mcpToolInput()` in the example with the `mcpUpdates()` / restoration pattern:

```r
#' \dontrun{
#' mcpConfigure(
#'   appId = "sales",
#'   description = "Show the interactive sales dashboard.",
#'   arguments = list(region = ellmer::type_string("Region to focus on"))
#' )
#'
#' server <- function(input, output, session) {
#'   # Post-open updates from the model
#'   observe({
#'     region <- mcpUpdates()$region
#'     if (!is.null(region)) updateSelectInput(session, "region", selected = region)
#'   })
#'   mcpUpdateModelContext(text = paste("Viewing", input$region))
#' }
#' }
```

Update the file-header comments in `mcp-server.R`, `mcp-app.R`, `mcp-stdio.R` that reference `options(shiny.mcp*)`.

- [ ] **Step 3: Regenerate docs + NAMESPACE**

Run: `R -q -e 'devtools::document()'`
Expected: `NAMESPACE` gains `export(mcpConfigure)` and `export(mcpUpdates)`, loses `export(mcpToolInput)`; `man/mcpConfigure.Rd` and `man/mcp-session.Rd` regenerate.

- [ ] **Step 4: Add a NEWS bullet**

Add under the current dev version heading in `NEWS.md`:

```markdown
* MCP App support is now configured with `mcpConfigure()` instead of
  `options(shiny.mcp.*)`, and the session helper `mcpToolInput()` is renamed
  to `mcpUpdates()`. (#XXXX)
```

- [ ] **Step 5: Verify + commit**

Run: `grep -rn "shiny.mcp\b\|shiny.mcp\.\|mcpToolInput" R/ mcp/ man/ NEWS.md`
Expected: only the three sendCustomMessage channel names (`shiny.mcp.updateModelContext`, `shiny.mcp.sendMessage`, `shiny.mcp.requestDisplayMode`) remain.

```bash
git add -A
git commit -m "docs(mcp): document mcpConfigure()/mcpUpdates(), update demos and NEWS"
```

- [ ] **Step 6: Full package check gate**

Run: `R -q -e 'devtools::test(filter = "mcp")'` then `R -q -e 'devtools::check(document = FALSE, args = "--no-manual")'`
Expected: tests pass; check has no new NOTES/WARNINGS about undocumented objects or bad `\usage`.

---

# PHASE B — Init arguments via `RestoreContext`

> Phase B changes runtime behavior: the model's *opening* arguments initialize inputs through Shiny's `RestoreContext` (flash-free), and `mcpUpdates()` becomes post-init-only. It lands in the same PR.

### Task B1: Spike — pin down the two unknowns

**Files:** none (produces `mcp/notes-restore-spike.md`)

Planning surfaced two facts that Phase B depends on; confirm both before coding.

- [ ] **Step 1: Confirm restore-observer gating**

Read `R/shiny.R:1931-2015` and `R/server.R:198-212`. Confirm: `onRestore`/`onRestored` observers are registered only inside `createBookmarkObservers()`, which early-returns when `bookmarkStore == "disable"` (`R/shiny.R:1936`), and `restoreInput()` (`R/bookmark-state.R:511`) returns defaults unless an **active** `RestoreContext` holds the input. Conclusion to record: to restore MCP init args without the app enabling bookmarking, the server must (a) build an *active* `RestoreContext` from the args and (b) register the restore-callback observers independently of `bookmarkStore`.

- [ ] **Step 2: Confirm the restore query-string encoding**

Read `R/bookmark-state.R` `decodeStateQueryString()` (the `_inputs_` / `_values_` grammar) and `RestoreInputSet`. Record the exact string the bridge must produce: `_inputs_&<name>=<url-encoded JSON>&...` — e.g. `_inputs_&n=200&note=%22hi%22`. Confirm `restoreInput("n")` returns `200` when the context is built from that string. Write a throwaway R snippet:

```r
devtools::load_all()
rc <- shiny:::RestoreContext$new("_inputs_&n=200&note=%22hi%22")
rc$active                 # expect TRUE
rc$input$get("n")         # expect 200
```

- [ ] **Step 3: Determine the ext-apps initial-tool-input API**

In `srcts/src/mcp/index.ts` and `@modelcontextprotocol/ext-apps`, determine how the **initial** tool-call arguments reach the iframe: whether they are available during/right after `ui/initialize` (so they can be encoded before `Shiny`'s socket `init` message is sent), or only arrive asynchronously via `app.ontoolinput`. Record the concrete API (e.g. an `App` field/method, or the first `ontoolinput` firing) and whether the bridge's existing `setTimeout(() => s.start(), 0)` deferral gives enough time. This decides Task B3's approach.

- [ ] **Step 4: Write findings to `mcp/notes-restore-spike.md` and commit**

```bash
git add mcp/notes-restore-spike.md
git commit -m "docs(mcp): spike notes for init-args-via-RestoreContext"
```

---

### Task B2: Server-side — active RestoreContext from MCP args

**Files:**
- Modify: `R/shiny.R` (extract `createRestoreObservers()` from `createBookmarkObservers()`)
- Modify: `R/server.R:198-212`
- Test: `tests/testthat/test-mcp-restore.R` (new)

**Interfaces:**
- Consumes: spike conclusion (build active context + register restore observers regardless of `bookmarkStore`).
- Produces: MCP `init` messages carrying `.clientdata_mcp_restore` (a `_inputs_&...` string) yield an active `RestoreContext` whose inputs feed `restoreInput()` and whose state feeds `onRestore()`.

- [ ] **Step 1: Write failing tests**

```r
# tests/testthat/test-mcp-restore.R
test_that("an MCP restore query string builds an active input context", {
  rc <- RestoreContext$new("_inputs_&n=200&note=%22hi%22")
  expect_true(rc$active)
  expect_equal(rc$input$get("n"), 200)
  expect_equal(rc$input$get("note"), "hi")
})

test_that("restore observers are registered for MCP sessions without bookmarking", {
  # Uses a MockShinySession-driven harness; assert that a session created with
  # bookmarkStore = "disable" but a non-empty .clientdata_mcp_restore ends up
  # with an active restoreContext and fires onRestore.
  # (Fill in with the project's session-init test harness from test-mcp-server.R.)
  skip("implement with the session-init harness confirmed in the B1 spike")
})
```

- [ ] **Step 2: Run to verify the first test fails only if encoding differs**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-restore.R")'`
Expected: first test PASSES if B1 Step 2 held (it exercises existing code); the skipped test documents the integration path.

- [ ] **Step 3: Extract `createRestoreObservers()`**

In `R/shiny.R`, move the three restore-related observers (init-error notification, `onRestore` invoke at `priority = 1000000`, and the `onFlushed` `onRestored` invoke — currently `R/shiny.R:1957-2015`) out of `createBookmarkObservers()` into a new private method `createRestoreObservers = function() { ... }`, and call it from the top of `createBookmarkObservers()` (after the `store == "disable"` guard) so existing bookmarking behavior is unchanged:

```r
createBookmarkObservers = function() {
  store <- getShinyOption("bookmarkStore", default = "disable")
  if (store == "disable") return()
  # ... existing save-to-server warning ...
  self$createRestoreObservers()
  withReactiveDomain(self, {
    observeEvent(self$input[["._bookmark_"]], { self$doBookmark() })
    # ... remaining SAVE-side observers only ...
  })
},

createRestoreObservers = function() {
  withReactiveDomain(self, {
    # (the three moved observers verbatim)
  })
},
```

- [ ] **Step 4: Wire MCP restore in `R/server.R:202-212`**

```r
if (is.null(shinysession$restoreContext)) {
  bookmarkStore <- getShinyOption("bookmarkStore", default = "disable")
  mcpRestore <- if (mcpEnabled()) msg$data$.clientdata_mcp_restore else NULL
  if (!is.null(mcpRestore) && nzchar(mcpRestore)) {
    # MCP opening args: restore inputs flash-free without requiring the app
    # to enable bookmarking. Build an active restore context and register
    # only the restore-side observers (no save-to-bookmark behavior).
    shinysession$restoreContext <- RestoreContext$new(mcpRestore)
    shinysession$createRestoreObservers()
  } else if (bookmarkStore == "disable") {
    shinysession$restoreContext <- RestoreContext$new()
  } else {
    shinysession$restoreContext <- RestoreContext$new(msg$data$.clientdata_url_search)
    shinysession$createBookmarkObservers()
  }
}
```

- [ ] **Step 5: Fill in and run the integration test**

Implement the skipped test using the session-init harness identified in B1, then:
Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-restore.R")'`
Expected: PASS.

- [ ] **Step 6: Regression-check bookmarking is unchanged**

Run: `R -q -e 'devtools::test(filter = "bookmark")'`
Expected: PASS (extraction is behavior-preserving).

- [ ] **Step 7: Commit**

```bash
git add R/shiny.R R/server.R tests/testthat/test-mcp-restore.R
git commit -m "feat(mcp): restore init arguments via an active RestoreContext"
```

---

### Task B3: Bridge — encode init args into restore clientdata

**Files:**
- Modify: `srcts/src/mcp/index.ts:270-274`
- Rebuild: `inst/www/shared/shiny-mcp-bridge.js`
- Test: `srcts/src/mcp/__tests__/` (encoding unit test) + reuse `test-mcp-restore.R`

**Interfaces:**
- Consumes: B1 Step 3's decision on when initial args are available; B2's `.clientdata_mcp_restore` contract.
- Produces: initial whitelisted args set as `.clientdata_mcp_restore` (bookmark `_inputs_` format) before socket start; post-init args set as `.clientdata_mcp_tool_input` (feeding `mcpUpdates()`).

- [ ] **Step 1: Add an encoder + a unit test**

```ts
// encode { n: 200, note: "hi" } -> "_inputs_&n=200&note=%22hi%22"
export function encodeMcpRestore(args: Record<string, unknown>): string {
  const parts = Object.entries(args).map(
    ([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(JSON.stringify(v))}`,
  );
  return "_inputs_&" + parts.join("&");
}
```

Add a test asserting `encodeMcpRestore({ n: 200, note: "hi" })` round-trips to the string B1 Step 2 confirmed `RestoreContext` decodes.

- [ ] **Step 2: Route init vs. update in `index.ts`**

Replace the single `app.ontoolinput` handler (`srcts/src/mcp/index.ts:270-274`) so the **first** tool input (per B1 Step 3's timing) is encoded to `.clientdata_mcp_restore` before the socket starts, and **subsequent** inputs go to `.clientdata_mcp_tool_input`:

```ts
let toolInputSeen = false;
app.ontoolinput = (params) => {
  const args = params.arguments ?? {};
  if (!toolInputSeen) {
    toolInputSeen = true;
    // init: feed Shiny's RestoreContext (applied before the server runs)
    setShinyInput(".clientdata_mcp_restore", encodeMcpRestore(args));
  } else {
    // post-init update: feed mcpUpdates()
    setShinyInput(".clientdata_mcp_tool_input", JSON.stringify(args));
  }
};
```

(If B1 Step 3 finds initial args are NOT available before socket start, implement the deferral it prescribes — e.g. hold `socket.start()` until the first `ontoolinput` or a short timeout — and record that in the spike note.)

- [ ] **Step 3: Rebuild the bundle**

Run: `npm run bundle_mcp`
Expected: `inst/www/shared/shiny-mcp-bridge.js` updated (appears in `git status`).

- [ ] **Step 4: Run JS + R tests**

Run: `npm test -- mcp` (or the project's JS test command) and `R -q -e 'devtools::test(filter = "mcp")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add srcts/src/mcp/index.ts srcts/src/mcp/__tests__ inst/www/shared/shiny-mcp-bridge.js
git commit -m "feat(mcp): bridge encodes init args into restore, updates via mcpUpdates()"
```

---

### Task B4: Whitelist filtering + docs

**Files:**
- Modify: `R/mcp-config.R` (add `mcpFilterArguments()`) and its call site
- Modify: `mcp/design-mcpConfigure.md`, `mcp/demo-app/app.R` (show restore + update pattern)
- Test: `tests/testthat/test-mcp-config.R`

- [ ] **Step 1: Write a failing test**

```r
test_that("only declared arguments are kept", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  expect_equal(mcpFilterArguments(list(n = 5, evil = "x")), list(n = 5))
})

test_that("with no declared arguments nothing passes the filter", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure()
  expect_equal(mcpFilterArguments(list(n = 5)), list())
})
```

- [ ] **Step 2: Implement the filter**

```r
# Keep only arguments the app declared in mcpConfigure(arguments = ).
mcpFilterArguments <- function(args) {
  declared <- names(.globals$mcp$arguments)
  if (is.null(declared) || is.null(args)) return(list())
  args[intersect(names(args), declared)]
}
```

- [ ] **Step 3: Apply the filter where MCP args enter**

Filter in the two entry points: before encoding restore in the bridge is not R-side, so enforce server-side — apply `mcpFilterArguments()` to `mcpParseClientData(session$clientData$mcp_tool_input)` inside `mcpUpdates()` (post-init), and validate/filter `.clientdata_mcp_restore` decoding server-side. Update `mcpUpdates()`:

```r
mcpUpdates <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("mcpUpdates() must be called from within a Shiny session")
  }
  mcpFilterArguments(mcpParseClientData(session$clientData$mcp_tool_input))
}
```

- [ ] **Step 4: Run tests**

Run: `R -q -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-config.R")'`
Expected: PASS.

- [ ] **Step 5: Update the demo + spec, commit**

Add to `mcp/demo-app/app.R` server the init (restore) + update (mcpUpdates) pattern for `n`, and note in `mcp/design-mcpConfigure.md` that Phase B is implemented. Then:

```bash
git add R/mcp-config.R R/mcp-session.R mcp/demo-app/app.R mcp/design-mcpConfigure.md tests/testthat/test-mcp-config.R
git commit -m "feat(mcp): filter model arguments to the declared allow-list"
```

- [ ] **Step 6: Final full check**

Run: `R -q -e 'devtools::test()'` then `R -q -e 'devtools::check(args = "--no-manual")'`
Expected: green.

---

## Self-Review

**Spec coverage:**
- `mcpConfigure()` signature + validation → A1. ✅
- Opt-in safeguard (no call = disabled; `enabled` last) → A1 (tests) + A2 (`mcpEnabled`). ✅
- `.globals$mcp` storage + accessor rewrite → A1/A2. ✅
- Tool name from `appId` → A1 (`mcpToolName`) + A2 (`mcpToolInfo`). ✅
- `arguments` as ellmer types → JSON Schema → A1 (`mcpArgumentsSchema`) + A2. ✅
- `mcpToolInput()` → `mcpUpdates()` → A3. ✅
- Remove all options + docs/demos/NEWS → A5. ✅
- Init via RestoreContext (flash-free) → B2/B3. ✅
- After-init via `mcpUpdates()` → A3 + B3 (routing). ✅
- Whitelist filtering → B4. ✅
- Testing plan → A1–A4, B2–B4. ✅

**Placeholder scan:** Two intentional deferrals live in Task B1 (a spike whose entire purpose is to resolve them) and are consumed by B2/B3; every other step carries concrete code/commands. The one `skip()`ed test in B2 is explicitly filled in at B2 Step 5 after the spike identifies the harness.

**Type consistency:** `mcpConfigure()` arg names, `.globals$mcp` field names, `mcpToolName()`, `mcpArgumentsSchema()`, `mcpFilterArguments()`, `mcpUpdates()`, and `.clientdata_mcp_restore` / `.clientdata_mcp_tool_input` are used consistently across tasks.
