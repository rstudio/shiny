# registerMcpTool() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `options(shiny.mcp.tools = list(...))` with an exported `registerMcpTool()` function that accepts `ellmer::tool()` objects as MCP author-tool definitions.

**Architecture:** Author tools are stored in process-global `.globals$mcpAuthorTools` (named list keyed by tool name). `registerMcpTool()` validates and registers `ellmer::ToolDef` objects eagerly. `tools/list` converts each ToolDef's typed `@arguments` to JSON Schema via the same dummy-provider `as_json()` path `mcptools` uses; `tools/call` invokes the ToolDef with `rlang::exec()`. Return-value handling is unchanged.

**Tech Stack:** R, ellmer (Suggests), rlang (Imports, already present), testthat 3e, withr.

## Global Constraints

- Scope is **author tools only**. The app-opening tool (`options(shiny.mcp.tool)`, read by `mcpToolInfo()`) and every other `shiny.mcp.*` option stay unchanged.
- `options(shiny.mcp.tools = ...)` is **removed entirely** — no fallback. `registerMcpTool()` is the only path.
- `ellmer` is a **Suggests** dependency, pinned `>= 0.4.0`. Gate use with `rlang::check_installed("ellmer")`.
- Reuse the `mcptools::tool_as_json()` conversion verbatim (`getNamespace("ellmer")[["as_json"]]` + a dummy `ellmer::Provider()`) to stay upstream-aligned.
- Registration is **last-write-wins by name** (no error on duplicate, no `force` arg).
- All new/changed MCP author-tool tests are gated with `skip_if_not_installed("ellmer")`.
- Reference the design at `mcp/design-register-mcp-tool.md`.

---

### Task 1: `registerMcpTool()` — registration + eager validation

**Files:**
- Create: `R/mcp-tools.R`
- Modify: `R/globals.R` (initialise `.globals$mcpAuthorTools`)
- Modify: `R/mcp-server.R` (add `mcpReservedToolNames()` helper)
- Modify: `DESCRIPTION` (add `ellmer` to Suggests)
- Test: `tests/testthat/test-mcp-tools.R` (new)
- Test: `tests/testthat/helper-mcp.R` (add `local_mcp_tools()`)

**Interfaces:**
- Produces: `registerMcpTool(...)` — exported; accepts one or more `ellmer::ToolDef` objects; writes each to `.globals$mcpAuthorTools[[tool@name]]`; returns `invisible()`.
- Produces: `mcpReservedToolNames()` — internal; returns `character()` of names an author tool may not use (`mcpToolInfo()$name` plus the `_shiny_*` tunnel tool names).
- Produces: `.globals$mcpAuthorTools` — named list of `ellmer::ToolDef`, keyed by name.

- [ ] **Step 1: Add the test-isolation helper**

In `tests/testthat/helper-mcp.R`, append:

```r
# Reset registered MCP author tools around a test (registerMcpTool() writes
# to process-global .globals$mcpAuthorTools).
local_mcp_tools <- function(env = parent.frame()) {
  old <- .globals$mcpAuthorTools
  withr::defer(.globals$mcpAuthorTools <- old, envir = env)
  .globals$mcpAuthorTools <- list()
}
```

- [ ] **Step 2: Write the failing tests**

Create `tests/testthat/test-mcp-tools.R`:

```r
test_that("registerMcpTool() stores ellmer tools by name (last-write-wins)", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()

  registerMcpTool(
    ellmer::tool(function(n) n, name = "echo_n",
      description = "Echo n.",
      arguments = list(n = ellmer::type_integer("n", required = TRUE)))
  )
  expect_named(mcpAuthorTools(), "echo_n")
  expect_true(inherits(mcpAuthorTools()[["echo_n"]], "ellmer::ToolDef"))

  # Re-registering the same name replaces, does not error or duplicate.
  registerMcpTool(
    ellmer::tool(function(n) n * 2, name = "echo_n",
      description = "Echo 2n.",
      arguments = list(n = ellmer::type_integer("n", required = TRUE)))
  )
  expect_length(mcpAuthorTools(), 1)
  expect_equal(mcpAuthorTools()[["echo_n"]]@description, "Echo 2n.")
})

test_that("registerMcpTool() accepts multiple tools in one call", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(function() 1, name = "a", description = "a"),
    ellmer::tool(function() 2, name = "b", description = "b")
  )
  expect_setequal(names(mcpAuthorTools()), c("a", "b"))
})

test_that("registerMcpTool() rejects non-ToolDef arguments", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  expect_error(
    registerMcpTool(list(name = "x", handler = function(args) 1)),
    "ellmer::tool"
  )
})

test_that("registerMcpTool() rejects reserved tool names", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  expect_error(
    registerMcpTool(
      ellmer::tool(function() 1, name = "_shiny_send", description = "no")
    ),
    "reserved"
  )
})
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-tools.R")'`
Expected: FAIL — `could not find function "registerMcpTool"`.

- [ ] **Step 4: Initialise the global state**

In `R/globals.R`, immediately after `.globals <- new.env(parent = emptyenv())`, add:

```r
.globals$mcpAuthorTools <- list()
```

- [ ] **Step 5: Add the reserved-names helper**

In `R/mcp-server.R`, add (near `mcpToolInfo()`):

```r
# Tool names an author may not reuse: the app-opening tool and the internal
# _shiny_* tunnel tools.
mcpReservedToolNames <- function() {
  c(
    mcpToolInfo()$name,
    vapply(mcpTunnelToolsList(), function(t) t$name, character(1))
  )
}
```

- [ ] **Step 6: Write `registerMcpTool()`**

Create `R/mcp-tools.R`:

```r
#' Register an MCP tool the model can call directly
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' When a Shiny app is served as an MCP App (`options(shiny.mcp = TRUE)`),
#' `registerMcpTool()` exposes plain R functions as MCP tools the model may
#' call directly, in addition to the tool that opens the app. Define each
#' tool with [ellmer::tool()] and register it at the top level of your
#' `app.R`, before the app starts:
#'
#' ```r
#' library(shiny)
#' options(shiny.mcp = TRUE)
#'
#' registerMcpTool(
#'   ellmer::tool(
#'     function(n) {
#'       x <- rnorm(n)
#'       list(n = n, mean = mean(x), sd = stats::sd(x))
#'     },
#'     name = "get_sample_stats",
#'     description = "Summary statistics for a normal sample of size n.",
#'     arguments = list(n = ellmer::type_integer("Sample size", required = TRUE))
#'   )
#' )
#' ```
#'
#' Handlers run in the server R process, outside of any Shiny session, and
#' may return a character vector (sent as text), a list (sent as JSON
#' `structuredContent`), or a promise. Errors are reported to the model as
#' tool errors.
#'
#' Tools are keyed by their `name`; registering a tool whose name is already
#' registered replaces it. A tool may not use a name reserved by Shiny's MCP
#' server (the app-opening tool or the internal `_shiny_*` tunnel tools).
#'
#' @param ... One or more tools created with [ellmer::tool()].
#'
#' @return Invisibly, `NULL`.
#'
#' @seealso [mcpToolInput()] and the other `mcp*` session helpers.
#' @export
registerMcpTool <- function(...) {
  rlang::check_installed("ellmer", "to define MCP tools with `ellmer::tool()`.")
  tools <- list(...)
  reserved <- mcpReservedToolNames()
  for (tool in tools) {
    if (!inherits(tool, "ellmer::ToolDef")) {
      stop(
        "`registerMcpTool()` accepts tools created with `ellmer::tool()`; ",
        "received an object of class '", class(tool)[1], "'.",
        call. = FALSE
      )
    }
    name <- tool@name
    if (name %in% reserved) {
      stop(
        "Cannot register an MCP tool named '", name, "': that name is ",
        "reserved by Shiny's MCP server.",
        call. = FALSE
      )
    }
    .globals$mcpAuthorTools[[name]] <- tool
  }
  invisible()
}
```

- [ ] **Step 7: Add ellmer to Suggests**

In `DESCRIPTION`, add to the `Suggests:` list (keep alphabetical order):

```
    ellmer (>= 0.4.0),
```

- [ ] **Step 8: Regenerate docs**

Run: `R -q -e 'devtools::document()'`
Expected: `NAMESPACE` gains `export(registerMcpTool)`; `man/registerMcpTool.Rd` created.

- [ ] **Step 9: Run tests to verify they pass**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-tools.R")'`
Expected: PASS (4 tests). `mcpAuthorTools()` is exercised as a reader here; Task 2 changes its body.

- [ ] **Step 10: Commit**

```bash
git add R/mcp-tools.R R/globals.R R/mcp-server.R DESCRIPTION NAMESPACE man/registerMcpTool.Rd tests/testthat/test-mcp-tools.R tests/testthat/helper-mcp.R
git commit -m "feat: registerMcpTool() registers ellmer tools as MCP author tools"
```

---

### Task 2: Convert registered ellmer tools into `tools/list` JSON Schema

**Files:**
- Modify: `R/mcp-server.R` (`mcpAuthorTools()`, `mcpToolsList` author-tool block; add `mcpToolInputSchema()`)
- Modify: `tests/testthat/test-mcp-server.R` (replace the option-based `tools/list` test)

**Interfaces:**
- Consumes: `.globals$mcpAuthorTools` and `mcpReservedToolNames()` from Task 1.
- Produces: `mcpAuthorTools()` — internal; now returns `.globals$mcpAuthorTools %||% list()` (no `warn` parameter).
- Produces: `mcpToolInputSchema(tool)` — internal; returns a JSON Schema list for an `ellmer::ToolDef`'s `@arguments`.

- [ ] **Step 1: Write the failing test**

In `tests/testthat/test-mcp-server.R`, **replace** the test `"author-declared tools appear in tools/list and execute"` (the `tools/list` half) with:

```r
test_that("registered author tools appear in tools/list with JSON schema", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(
      function(x, y) list(sum = x + y),
      name = "add_numbers",
      description = "Add two numbers.",
      arguments = list(
        x = ellmer::type_number("x", required = TRUE),
        y = ellmer::type_number("y", required = TRUE)
      )
    ),
    ellmer::tool(function() "Hello from R", name = "get_motd",
      description = "Get the message of the day.")
  )
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  out <- mcp_post(h, "tools/list")

  # tools must serialize as a JSON array, not an object
  expect_null(names(out$result$tools))
  tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
  expect_true(all(c("add_numbers", "get_motd") %in% tool_names))

  add_tool <- out$result$tools[[which(tool_names == "add_numbers")]]
  expect_equal(add_tool$inputSchema$type, "object")
  expect_equal(add_tool$inputSchema$properties$x$type, "number")
  # required must be a JSON array even when derived from ellmer
  expect_equal(add_tool$inputSchema$required, list("x", "y"))
  expect_null(add_tool$inputSchema$description)

  # a tool with no arguments still advertises an object schema with properties
  motd <- out$result$tools[[which(tool_names == "get_motd")]]
  expect_equal(motd$inputSchema$type %||% "object", "object")
})
```

Note: `mcp_post()` returns parsed-from-JSON structures, so `required` arrives as a `list`. This is the raw-JSON assertion that catches the named-list-serializes-as-object trap.

- [ ] **Step 2: Run test to verify it fails**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-server.R", desc = "registered author tools appear in tools/list with JSON schema")'`
Expected: FAIL — old `mcpAuthorTools()` still reads the removed option / no conversion, so `add_numbers` is absent.

- [ ] **Step 3: Add the schema-conversion helper**

In `R/mcp-server.R`, add near `mcpAuthorTools()`:

```r
# Convert an ellmer ToolDef's typed @arguments into a JSON Schema list for
# tools/list. Mirrors mcptools::tool_as_json() so the two stay aligned: a
# dummy provider drives ellmer's provider-agnostic base as_json() method.
mcpToolInputSchema <- function(tool) {
  as_json <- getNamespace("ellmer")[["as_json"]]
  schema <- as_json(ellmer::Provider("dummy", "dummy", "dummy"), tool@arguments)
  schema$description <- NULL
  if (is.null(schema$properties)) {
    schema$properties <- empty_named_list()
  }
  dropNulls(schema)
}
```

- [ ] **Step 4: Simplify `mcpAuthorTools()` to read the global**

In `R/mcp-server.R`, **replace** the entire `mcpAuthorTools <- function(warn = FALSE) { ... }` definition (and its comment block) with:

```r
# Author-declared tools registered via registerMcpTool(). Named list of
# ellmer::ToolDef objects, keyed by tool name; validation happens eagerly at
# registration time.
mcpAuthorTools <- function() {
  .globals$mcpAuthorTools %||% list()
}
```

- [ ] **Step 5: Update the `tools/list` author-tool block**

In `R/mcp-server.R` `mcpToolsList` (the `unname(c(...))` builder), **replace** the author-tool `lapply`:

```r
    lapply(mcpAuthorTools(warn = TRUE), function(tool) {
      list(
        name = tool$name,
        description = tool$description,
        inputSchema = tool$inputSchema %||%
          list(type = "object", properties = empty_named_list())
      )
    })
```

with:

```r
    lapply(mcpAuthorTools(), function(tool) {
      dropNulls(list(
        name = tool@name,
        title = tool@annotations$title,
        description = tool@description,
        inputSchema = mcpToolInputSchema(tool)
      ))
    })
```

- [ ] **Step 6: Run test to verify it passes**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-server.R", desc = "registered author tools appear in tools/list with JSON schema")'`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add R/mcp-server.R tests/testthat/test-mcp-server.R
git commit -m "feat: convert registered ellmer tools to MCP tools/list schema"
```

---

### Task 3: Invoke registered tools on `tools/call`

**Files:**
- Modify: `R/mcp-server.R` (`mcpAuthorToolCall()`)
- Modify: `tests/testthat/test-mcp-server.R` (replace the remaining option-based execute/error/async/collision tests)

**Interfaces:**
- Consumes: `mcpAuthorTools()` (returns ToolDefs) and `mcpToolCall()`'s existing lookup `mcpAuthorTools()[[name]]`.
- Produces: `mcpAuthorToolCall(tool, params, id)` — now calls the ToolDef via `rlang::exec()`.

- [ ] **Step 1: Write the failing tests**

In `tests/testthat/test-mcp-server.R`, **add** the execute half back and **replace** the error/async/collision tests:

```r
test_that("registered author tools execute via tools/call", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(function(x, y) list(sum = x + y), name = "add_numbers",
      description = "Add two numbers.",
      arguments = list(
        x = ellmer::type_number("x", required = TRUE),
        y = ellmer::type_number("y", required = TRUE)
      )),
    ellmer::tool(function() "Hello from R", name = "get_motd",
      description = "Get the message of the day.")
  )
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)

  res <- mcp_post(h, "tools/call",
    params = list(name = "add_numbers", arguments = list(x = 2, y = 40)))
  expect_equal(res$result$structuredContent$sum, 42)
  expect_match(res$result$content[[1]]$text, "42")

  res <- mcp_post(h, "tools/call",
    params = list(name = "get_motd", arguments = empty_named_list()))
  expect_equal(res$result$content[[1]]$text, "Hello from R")
  expect_null(res$result$structuredContent)
})

test_that("author tool errors become isError results, not crashes", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(function(why) stop("kaboom: ", why), name = "boom",
      description = "Always fails.",
      arguments = list(why = ellmer::type_string("why", required = TRUE)))
  )
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  res <- mcp_post(h, "tools/call",
    params = list(name = "boom", arguments = list(why = "testing")))
  expect_true(isTRUE(res$result$isError))
  expect_match(res$result$content[[1]]$text, "kaboom: testing")
})

test_that("async author tools (promises) are supported", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(
      function() promises::promise(function(resolve, reject) {
        later::later(function() resolve(list(answer = 42)), 0.1)
      }),
      name = "slow_answer", description = "Answers later.")
  )
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  res <- mcp_post(h, "tools/call",
    params = list(name = "slow_answer", arguments = empty_named_list()))
  expect_equal(res$result$structuredContent$answer, 42)
})
```

Then **delete** the old test `"invalid or colliding author tools are skipped with a warning"` — collision/validation is now eager and covered by Task 1's `test-mcp-tools.R`.

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-server.R")'`
Expected: FAIL on the execute/error/async tests — `mcpAuthorToolCall()` still calls `tool$handler(args)`, which is `NULL` for a ToolDef.

- [ ] **Step 3: Update `mcpAuthorToolCall()`**

In `R/mcp-server.R`, **replace** the `hybrid_chain(...)` seed line in `mcpAuthorToolCall()`:

```r
mcpAuthorToolCall <- function(tool, params, id) {
  args <- params$arguments %||% empty_named_list()
  hybrid_chain(
    tryCatch(tool$handler(args), error = function(e) e),
```

with:

```r
mcpAuthorToolCall <- function(tool, params, id) {
  args <- params$arguments %||% list()
  hybrid_chain(
    tryCatch(rlang::exec(tool, !!!args), error = function(e) e),
```

(The rest of the function — result/error handling — is unchanged.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-mcp-server.R")'`
Expected: PASS (all remaining tests in the file).

- [ ] **Step 5: Full MCP test sweep**

Run: `R -q -e 'pkgload::load_all(); testthat::test_dir("tests/testthat", filter = "mcp")'`
Expected: PASS — confirms no other MCP test referenced `shiny.mcp.tools`.

- [ ] **Step 6: Commit**

```bash
git add R/mcp-server.R tests/testthat/test-mcp-server.R
git commit -m "feat: invoke registered ellmer tools on MCP tools/call"
```

---

### Task 4: Documentation — session help, pkgdown, NEWS

**Files:**
- Modify: `R/mcp-session.R` (rewrite the "Additional tools" `@section`)
- Modify: `tools/documentation/pkgdown.yml` (add `registerMcpTool` to the MCP Apps section)
- Modify: `NEWS.md`
- Regenerate: `man/`

**Interfaces:** none (docs only).

- [ ] **Step 1: Rewrite the "Additional tools" section**

In `R/mcp-session.R`, **replace** the `@section Additional tools:` block (currently describing `options(shiny.mcp.tools = list(...))` and its `list(name, description, inputSchema, handler)` example, lines ~53-80) with:

```r
#' @section Additional tools:
#' Beyond the tool that opens the app, authors can expose plain R functions
#' as MCP tools the model may call directly, with [registerMcpTool()] and
#' [ellmer::tool()]. See `?registerMcpTool` for details.
```

- [ ] **Step 2: Add the pkgdown reference entry**

In `tools/documentation/pkgdown.yml`, under the existing "MCP Apps" section's `contents:`, add:

```yaml
      - registerMcpTool
```

- [ ] **Step 3: Update NEWS**

In `NEWS.md`, locate the bullet added for the MCP Apps feature (referencing `#4407`) and ensure it names the author-tool API. If a distinct bullet is warranted, add under the current development version:

```
* Shiny apps served as MCP Apps can expose additional model-callable tools
  with `registerMcpTool()`, using `ellmer::tool()` definitions (#4407).
```

- [ ] **Step 4: Regenerate docs**

Run: `R -q -e 'devtools::document()'`
Expected: `man/mcp-session.Rd` and `man/registerMcpTool.Rd` updated with cross-links; no errors.

- [ ] **Step 5: Verify examples/links resolve**

Run: `R -q -e 'pkgload::load_all(); tools::checkRd("man/registerMcpTool.Rd"); tools::checkRd("man/mcp-session.Rd")'`
Expected: no `checkRd` warnings.

- [ ] **Step 6: Commit**

```bash
git add R/mcp-session.R tools/documentation/pkgdown.yml NEWS.md man/
git commit -m "docs: document registerMcpTool() and drop shiny.mcp.tools option"
```

---

### Task 5: Migrate demo apps and architecture notes

**Files:**
- Modify: `mcp/demo-app/app.R` (and any file setting `options(shiny.mcp.tools = ...)`)
- Modify: `mcp/demo-app2/app.R` (same)
- Modify: `mcp/architecture.md` (author-tools description)

**Interfaces:** none (docs/demos).

- [ ] **Step 1: Find the option usages in the demos**

Run: `grep -rn "shiny.mcp.tools" mcp/`
Expected: hits in `mcp/demo-app` and/or `mcp/demo-app2`. Record each block.

- [ ] **Step 2: Rewrite each `options(shiny.mcp.tools = ...)` as `registerMcpTool()`**

For each demo, convert the option's `list(name=, description=, inputSchema=, handler=)` spec into `registerMcpTool(ellmer::tool(<handler-body>, name=, description=, arguments=<ellmer types>))`. Example (get_sample_stats), replacing:

```r
options(shiny.mcp.tools = list(list(
  name = "get_sample_stats",
  description = "Summary statistics for a normal sample of size n.",
  inputSchema = list(type = "object",
    properties = list(n = list(type = "integer")), required = list("n")),
  handler = function(args) {
    x <- rnorm(args$n); list(n = args$n, mean = mean(x), sd = stats::sd(x))
  }
)))
```

with:

```r
registerMcpTool(
  ellmer::tool(
    function(n) { x <- rnorm(n); list(n = n, mean = mean(x), sd = stats::sd(x)) },
    name = "get_sample_stats",
    description = "Summary statistics for a normal sample of size n.",
    arguments = list(n = ellmer::type_integer("Sample size", required = TRUE))
  )
)
```

Convert `demo-app2`'s tool(s) the same way (map each `inputSchema` property to the matching `ellmer::type_*()`, and change the handler signature from `function(args)` reading `args$foo` to named parameters `function(foo, ...)`).

- [ ] **Step 3: Smoke-test one demo boots and lists the tool**

Run: `grep -rn "shiny.mcp.tools" mcp/`
Expected: no matches remaining.

Run (from repo root, adjust app path):
```bash
R -q -e 'pkgload::load_all(); options(shiny.mcp = TRUE); source("mcp/demo-app/app.R", local = new.env()); cat(vapply(shiny:::mcpAuthorTools(), function(t) t@name, character(1)), "\n")'
```
Expected: prints the demo's tool name(s) (e.g. `get_sample_stats`), confirming `registerMcpTool()` ran at source time.

- [ ] **Step 4: Update `mcp/architecture.md`**

In `mcp/architecture.md`, update the two author-tool references (the bullet near line 74 and the table row near line 220) from `options(shiny.mcp.tools = list(...))` to `registerMcpTool(ellmer::tool(...))`, noting the ellmer→JSON-Schema conversion mirrors `mcptools::tool_as_json()`.

- [ ] **Step 5: Commit**

```bash
git add mcp/demo-app mcp/demo-app2 mcp/architecture.md
git commit -m "docs(mcp): migrate demo apps and architecture to registerMcpTool()"
```

---

## Self-Review

**Spec coverage:**
- Public API `registerMcpTool(...)`, `...` of ToolDefs, invisible return → Task 1.
- Name rationale (mirrors registerInputHandler) → documented in spec; name used throughout.
- `.globals$mcpAuthorTools`, last-write-wins → Task 1 (Steps 4, 6; test in Step 2).
- Eager fail-fast validation (non-ToolDef, reserved name) → Task 1 (Step 6; tests Step 2).
- Drop lazy warn path → Task 2 Step 4 (mcpAuthorTools simplified); old warn test deleted Task 3 Step 1.
- Schema conversion via dummy-provider `as_json` → Task 2 Step 3, matching `mcptools::tool_as_json()`.
- `title` pass-through → Task 2 Step 5.
- Handler via `rlang::exec` + unchanged return handling → Task 3 Step 3.
- ellmer → Suggests (>= 0.4.0), `rlang::check_installed` → Task 1 Steps 6-7.
- Files touched (mcp-server, new mcp-tools, mcp-session section, DESCRIPTION, NAMESPACE, man, pkgdown, NEWS, demos, architecture) → Tasks 1-5.
- Tests gated with skip_if_not_installed → every author-tool test.
- Non-goals (no force/removeMcpTool, no option fallback, app tool untouched, no ContentToolResult) → respected; app tool via `mcpToolInfo()` left alone.

**Placeholder scan:** none — all code and commands are concrete.

**Type consistency:** `registerMcpTool` / `mcpAuthorTools()` (no args) / `mcpReservedToolNames()` / `mcpToolInputSchema(tool)` / `mcpAuthorToolCall(tool, params, id)` used consistently. ToolDefs accessed with `@name`, `@description`, `@arguments`, `@annotations$title` throughout.
