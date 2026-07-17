# In-place Update of a Running Shiny MCP App — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Auto-register an `update_<appId>_app` MCP tool that pushes new
arguments into an already-running Shiny session's `mcpUpdates()` channel, so a
model can update an open app in place instead of re-opening it.

**Architecture:** A plain `tools/call update_<appId>_app` runs in the server R
process. It takes a required `session` token, looks the session up in the
existing `appsByToken` registry, verifies `isMcpSession()`, filters the args
to the declared allow-list, and sets a per-session server-push `reactiveVal`
inside that session's reactive domain. `mcpUpdates()` is rewritten to overlay
that server-pushed value on top of the client-delivered init args. The app's
existing single `observe(mcpUpdates())` fires with no code change. The model
learns the token via a connect-time auto-emit through the existing
`shiny.mcp.updateModelContext` bridge channel; `mcpUpdateModelContext()`'s
payload gains a reserved `session` key.

**Tech Stack:** R (shiny package internals), testthat 3e, ellmer (optional,
`skip_if_not_installed`), existing MCP test harness in
`tests/testthat/helper-mcp.R` and `test-mcp-session.R`.

## Global Constraints

- No new hard package dependencies. `ellmer` stays optional — tests that need
  it call `skip_if_not_installed("ellmer")`; runtime code guards with
  `rlang::check_installed()` where already established.
- The feature is `lifecycle::badge("experimental")`, matching the rest of the
  `mcp*` surface.
- All new code is **R-side only**. No changes to `srcts/` — the bridge already
  forwards `shiny.mcp.updateModelContext` messages to the host verbatim.
- The `update_*` tool is published **only when `mcpConfigure(arguments = ...)`
  is declared** (`length(.globals$mcp$arguments) > 0`). No args ⇒ no tool.
- Targeting is by **explicit `session` token only** — no broadcast /
  most-recent fallback. Unknown/stale token returns a tool **error**
  (`isError = TRUE`), never a silent success.
- `session$token` equals the client's `config.sessionId` (see
  `R/shiny.R:1015`) and is the `appsByToken` key (see `R/server.R:176`).
- Per repo convention (see memory + existing `mcp/` docs), any MCP doc changes
  are mirrored into the repo-root `mcp/` copies.
- Reference spec: `mcp/design-mcp-update-running-app.md`.

**Run tests with:** `R -q -e 'devtools::test(filter = "<name>")'` where
`<name>` matches `tests/testthat/test-<name>.R` (e.g. `mcp-server`,
`mcp-session`). For a single test during development:
`R -q -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-mcp-session.R")'`.

---

## File Structure

- `R/mcp-config.R` — add `mcpUpdateToolName()` next to `mcpToolName()`.
- `R/mcp-server.R` — add `mcpUpdateToolSchema()`, the `tools/list` entry, the
  reserved-name entry, and the `update_*` dispatch branch + handler
  (`mcpUpdateAppCall()`).
- `R/mcp-session.R` — add the per-session server-push `reactiveVal` store
  (`mcpSessionUpdates` map + `mcpServerUpdatesFor()`), rewrite `mcpUpdates()`
  to overlay, reshape `mcpUpdateModelContext()`'s envelope, and add
  `mcpAnnounceSession()`.
- `R/server.R` — one guarded line in the websocket handler to trigger the
  connect-time token announcement for MCP sessions.
- `tests/testthat/test-mcp-server.R`, `test-mcp-session.R` — tests.
- `mcp/demo-app3/app.R` (+ `demo-app`, `demo-app2`) — comments only; verify
  zero-code-change works.
- Docs: `R/mcp-session.R` roxygen, `mcp/limitations.md`,
  `mcp/design-mcpConfigure.md`, `NEWS.md`.

---

## Task 1: `update_<appId>_app` tool name, schema, listing & reservation

**Files:**
- Modify: `R/mcp-config.R` (after `mcpToolName()`, ~line 133)
- Modify: `R/mcp-server.R` (`mcpToolsList()` ~335, `mcpReservedToolNames()` ~358)
- Test: `tests/testthat/test-mcp-server.R`

**Interfaces:**
- Produces: `mcpUpdateToolName()` → character; `mcpUpdateToolSchema()` → list
  (JSON Schema with a required `session` string plus the declared args).
- Consumes: `.globals$mcp$arguments`, `mcpArgumentsSchema()`,
  `mcpAppId()`/`.globals$mcp$appId`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-mcp-server.R`:

```r
test_that("update_<appId>_app is listed only when arguments are declared", {
  skip_if_not_installed("ellmer")

  # No arguments: no update tool.
  local_mcp_config(appId = "demo")
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  names_no_args <- vapply(
    mcp_post(h, "tools/list")$result$tools, function(t) t$name, character(1)
  )
  expect_false("update_demo_app" %in% names_no_args)

  # With arguments: update tool present, schema carries required `session`.
  local_mcp_config(
    appId = "demo",
    arguments = list(note = ellmer::type_string("A note"))
  )
  tools <- mcp_post(h, "tools/list")$result$tools
  nms <- vapply(tools, function(t) t$name, character(1))
  expect_true("update_demo_app" %in% nms)

  upd <- tools[[which(nms == "update_demo_app")]]
  expect_equal(upd$inputSchema$properties$session$type, "string")
  expect_equal(upd$inputSchema$properties$note$type, "string")
  expect_true("session" %in% unlist(upd$inputSchema$required))
  expect_match(upd$description, "in place", ignore.case = TRUE)
})

test_that("update tool name follows appId and reserves the name", {
  skip_if_not_installed("ellmer")
  local_mcp_config(arguments = list(x = ellmer::type_integer("x")))
  expect_equal(mcpUpdateToolName(), "update_shiny_app")
  expect_true("update_shiny_app" %in% mcpReservedToolNames())

  local_mcp_config(appId = "clock", arguments = list(x = ellmer::type_integer("x")))
  expect_equal(mcpUpdateToolName(), "update_clock_app")
  expect_true("update_clock_app" %in% mcpReservedToolNames())
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::test(filter = "mcp-server")'`
Expected: FAIL — `mcpUpdateToolName` not found / `update_demo_app` absent.

- [ ] **Step 3: Add `mcpUpdateToolName()`**

In `R/mcp-config.R`, after `mcpToolName()` (~line 133):

```r
# update_shiny_app, or update_<appId>_app when an appId is configured. The
# companion to mcpToolName(): the model calls this to change a running
# instance in place instead of re-opening the app.
mcpUpdateToolName <- function() {
  id <- .globals$mcp$appId
  if (is.null(id)) "update_shiny_app" else paste0("update_", id, "_app")
}
```

- [ ] **Step 4: Add `mcpUpdateToolSchema()` and list/reserve the tool**

In `R/mcp-server.R`, add near `mcpToolInfo()` (~line 199):

```r
# inputSchema for update_<appId>_app: the declared arguments allow-list plus a
# required `session` string identifying the running instance to update.
mcpUpdateToolInfo <- function() {
  schema <- mcpArgumentsSchema(.globals$mcp$arguments)
  schema$properties$session <- list(
    type = "string",
    description = paste(
      "Id of the running app instance to update, as reported by the app when",
      "it opened. Required."
    )
  )
  schema$required <- as.list(unique(c(unlist(schema$required), "session")))
  list(
    name = mcpUpdateToolName(),
    description = paste(
      "Change parameters of the already-open app in place. Do NOT re-open the",
      "app; use this to update the running instance identified by `session`."
    ),
    inputSchema = schema
  )
}

# TRUE when a companion update_* tool should be published (author declared
# arguments the model may push into a running session).
mcpHasUpdateTool <- function() {
  length(.globals$mcp$arguments) > 0
}
```

In `mcpToolsList()` (~line 335), splice the update entry after the open tool.
Change the `c(...)` to include it:

```r
mcpToolsList <- function() {
  info <- mcpToolInfo()
  update_entry <- if (mcpHasUpdateTool()) {
    upd <- mcpUpdateToolInfo()
    list(list(
      name = upd$name,
      description = upd$description,
      inputSchema = upd$inputSchema
    ))
  } else {
    list()
  }
  unname(c(
    list(list(
      name = info$name,
      description = info$description,
      inputSchema = info$inputSchema,
      `_meta` = list(ui = list(resourceUri = mcpResourceUri()))
    )),
    update_entry,
    mcpTunnelToolsList(),
    lapply(mcpAuthorTools(), function(tool) {
      dropNulls(list(
        name = S7::prop(tool, "name"),
        title = S7::prop(tool, "annotations")$title,
        description = S7::prop(tool, "description"),
        inputSchema = mcpToolInputSchema(tool)
      ))
    })
  ))
}
```

In `mcpReservedToolNames()` (~line 358):

```r
mcpReservedToolNames <- function() {
  c(
    mcpToolInfo()$name,
    if (mcpHasUpdateTool()) mcpUpdateToolName(),
    vapply(mcpTunnelToolsList(), function(t) t$name, character(1))
  )
}
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `R -q -e 'devtools::test(filter = "mcp-server")'`
Expected: PASS (all mcp-server tests green).

- [ ] **Step 6: Commit**

```bash
git add R/mcp-config.R R/mcp-server.R tests/testthat/test-mcp-server.R
git commit -m "feat(mcp): publish update_<appId>_app tool when arguments declared (#4415)"
```

---

## Task 2: Server-push `reactiveVal` store + `mcpUpdates()` overlay

**Files:**
- Modify: `R/mcp-session.R` (`mcpUpdates()` ~line 120; add store near top)
- Test: `tests/testthat/test-mcp-session.R`

**Interfaces:**
- Produces: `mcpServerUpdatesFor(session)` → the session's server-push
  `reactiveVal` (idempotent get-or-create, keyed by `session$token`, cleaned up
  on `session$onSessionEnded()`). `mcpUpdates(session)` now merges
  client init args + server-pushed args, then allow-list filters.
- Consumes: `mcpParseClientData()`, `mcpFilterArguments()`, `session$token`,
  `session$clientData$mcp_tool_input`, `session$onSessionEnded()`.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-mcp-session.R` (the harness `mcp_start_session`
already registers the session in `appsByToken`):

```r
test_that("mcpUpdates overlays server-pushed args on client init args", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(arguments = list(
    note = ellmer::type_string("a note"),
    n    = ellmer::type_integer("count")
  ))
  observed <- list()
  token <- NULL
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      token <<- session$token
      observe({
        val <- mcpUpdates()
        if (length(val) > 0) observed[[length(observed) + 1]] <<- val
      })
    }
  )
  # Client delivers the initial open args.
  mcp_send_update(sess, list(
    .clientdata_mcp_tool_input = '{"note":"hello","n":3}'
  ))
  pump_shiny(0.3)
  expect_equal(observed[[length(observed)]]$note, "hello")

  # Server pushes a partial update; `n` must persist, `note` changes. Set the
  # reactiveVal inside the session's domain, as the update handler does.
  session <- appsByToken$get(token)
  rv <- mcpServerUpdatesFor(session)
  withReactiveDomain(session, rv(list(note = "world")))
  session$requestFlush()
  pump_shiny(0.3)
  latest <- observed[[length(observed)]]
  expect_equal(latest$note, "world")
  expect_equal(latest$n, 3)

  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: FAIL — `mcpServerUpdatesFor` not found.

- [ ] **Step 3: Implement the store and rewrite `mcpUpdates()`**

In `R/mcp-session.R`, add near the top (after the roxygen block, before
`isMcpSession`):

```r
# Server-pushed MCP updates, keyed by session token. Shared between
# mcpUpdates() (a reactive read) and the update_<appId>_app tool handler (a
# server-side write): both fetch the *same* reactiveVal for a given session so
# a push invalidates the app's mcpUpdates() observer. Created lazily; removed
# when the session ends.
mcpSessionUpdates <- NULL
on_load({
  mcpSessionUpdates <- Map$new()
})

mcpServerUpdatesFor <- function(session) {
  token <- session$token
  rv <- mcpSessionUpdates$get(token)
  if (is.null(rv)) {
    rv <- reactiveVal(NULL, label = "mcpServerUpdates")
    mcpSessionUpdates$set(token, rv)
    session$onSessionEnded(function() mcpSessionUpdates$remove(token))
  }
  rv
}
```

Rewrite `mcpUpdates()` (~line 120):

```r
#' @rdname mcp-session
#' @export
mcpUpdates <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("mcpUpdates() must be called from within a Shiny session")
  }
  clientArgs <- mcpParseClientData(session$clientData$mcp_tool_input)
  # Server-side pushes from update_<appId>_app overlay the client-delivered
  # init args, per key (latest wins). Reading the reactiveVal registers the
  # dependency so a push re-fires this observer.
  serverArgs <- mcpServerUpdatesFor(session)()
  merged <- utils::modifyList(clientArgs %||% list(), serverArgs %||% list())
  mcpFilterArguments(merged)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: PASS. The existing `"mcpUpdates returns parsed tool arguments
reactively"` test must also still pass (client-only path unchanged when no
server push).

- [ ] **Step 5: Commit**

```bash
git add R/mcp-session.R tests/testthat/test-mcp-session.R
git commit -m "feat(mcp): mcpUpdates() overlays server-pushed args over init args (#4415)"
```

---

## Task 3: `update_<appId>_app` dispatch handler

**Files:**
- Modify: `R/mcp-server.R` (`mcpToolCall()` ~line 434; add `mcpUpdateAppCall()`)
- Test: `tests/testthat/test-mcp-session.R`

**Interfaces:**
- Consumes: `mcpUpdateToolName()`, `mcpHasUpdateTool()`, `appsByToken`,
  `isMcpSession()`, `mcpFilterArguments()`, `mcpServerUpdatesFor()`,
  `mcpToolErrorResult()`, `withReactiveDomain()`, `session$requestFlush()`.
- Produces: `mcpUpdateAppCall(params, id)` → an MCP tool result (success or
  `isError`).

- [ ] **Step 1: Write the failing tests**

Add to `tests/testthat/test-mcp-session.R`:

```r
test_that("update_<appId>_app pushes args into the running session", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(appId = "demo", arguments = list(
    note = ellmer::type_string("a note"),
    n    = ellmer::type_integer("count")
  ))
  observed <- list()
  token <- NULL
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      token <<- session$token
      observe({
        val <- mcpUpdates()
        if (length(val) > 0) observed[[length(observed) + 1]] <<- val
      })
    }
  )
  pump_shiny(0.2)

  h <- mcpHttpHandler(function(req) NULL, sess$handlers$ws)
  res <- mcp_post(h, "tools/call", params = list(
    name = "update_demo_app",
    # `session` and a non-declared key are filtered out before the push.
    arguments = list(session = token, note = "pushed", bogus = "x")
  ))
  expect_false(isTRUE(res$result$isError))
  pump_shiny(0.3)
  expect_equal(observed[[length(observed)]]$note, "pushed")

  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("update_<appId>_app errors on unknown or missing session token", {
  skip_if_not_installed("ellmer")
  local_mcp_config(appId = "demo", arguments = list(
    note = ellmer::type_string("a note")
  ))
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)

  missing <- mcp_post(h, "tools/call", params = list(
    name = "update_demo_app", arguments = list(note = "x")
  ))
  expect_true(missing$result$isError)

  unknown <- mcp_post(h, "tools/call", params = list(
    name = "update_demo_app",
    arguments = list(session = "no-such-token", note = "x")
  ))
  expect_true(unknown$result$isError)
  expect_match(unknown$result$content[[1]]$text, "no-such-token", fixed = TRUE)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: FAIL — dispatch returns "Unknown tool: update_demo_app".

- [ ] **Step 3: Implement the dispatch branch and handler**

In `R/mcp-server.R`, add the branch to `mcpToolCall()` right after the
open-tool check (after the `if (identical(name, info$name)) {...}` block,
~line 444):

```r
  if (mcpHasUpdateTool() && identical(name, mcpUpdateToolName())) {
    return(mcpUpdateAppCall(body$params, body$id))
  }
```

Add the handler nearby (e.g. after `mcpToolCall()`):

```r
# Handle a model call to update_<appId>_app: push the (allow-list-filtered)
# arguments into the running session named by `session`, inside that session's
# reactive domain so its mcpUpdates() observer re-fires. No re-render.
mcpUpdateAppCall <- function(params, id) {
  args <- params$arguments %||% list()
  token <- args$session
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    return(mcpToolErrorResult(
      id,
      "update requires a `session` id identifying the running app instance."
    ))
  }
  session <- appsByToken$get(token)
  if (is.null(session) || !isMcpSession(session)) {
    return(mcpToolErrorResult(id, sprintf(
      paste(
        "No running app session with id '%s'. Open the app first, or use the",
        "id the app reported for the instance you want to change."
      ),
      token
    )))
  }
  pushed <- mcpFilterArguments(args[setdiff(names(args), "session")])
  withReactiveDomain(session, {
    mcpServerUpdatesFor(session)(pushed)
  })
  session$requestFlush()
  mcpResult(id, list(content = list(list(
    type = "text",
    text = "Updated the running app in place."
  ))))
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/mcp-server.R tests/testthat/test-mcp-session.R
git commit -m "feat(mcp): dispatch update_<appId>_app to push args into a live session (#4415)"
```

---

## Task 4: `mcpUpdateModelContext()` reserved-key envelope

**Files:**
- Modify: `R/mcp-session.R` (`mcpUpdateModelContext()` ~line 148)
- Test: `tests/testthat/test-mcp-session.R` (update the existing test)

**Interfaces:**
- Produces: model-context messages whose `structuredContent` always carries a
  reserved `session` = `session$token`, with author `data` nested under `data`.
- Consumes: `session$token`, `dropNulls()`.

- [ ] **Step 1: Update the failing test**

Edit the existing test `"mcpUpdateModelContext and mcpSendMessage send bridge
messages"` in `tests/testthat/test-mcp-session.R` to assert the new shape.
After the existing `expect_match(frames, "structuredContent", fixed = TRUE)`,
add:

```r
  # The framework stamps the session token into structuredContent; author data
  # nests under `data`.
  expect_match(frames, "\"session\"", fixed = TRUE)
  expect_match(frames, "\"data\"", fixed = TRUE)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: FAIL — `"session"`/nested `"data"` not present in frames yet.

- [ ] **Step 3: Reshape the payload**

In `R/mcp-session.R`, `mcpUpdateModelContext()` (~line 158), replace the
`params <- dropNulls(...)` construction:

```r
  structured <- dropNulls(list(
    session = session$token,
    data = data
  ))
  params <- dropNulls(list(
    content = if (!is.null(text)) {
      list(list(type = "text", text = text))
    },
    structuredContent = structured
  ))
  session$sendCustomMessage("shiny.mcp.updateModelContext", params)
  invisible(TRUE)
```

> `session$token` is always available inside an MCP session (the function
> already returns early via `isMcpSession()` for non-MCP sessions), so
> `structured` always carries `session`; `data` is included only when the
> author passed it.

- [ ] **Step 4: Run test to verify it passes**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/mcp-session.R tests/testthat/test-mcp-session.R
git commit -m "feat(mcp): stamp session token into mcpUpdateModelContext payload (#4415)"
```

---

## Task 5: Connect-time token announcement

**Files:**
- Modify: `R/mcp-session.R` (add `mcpAnnounceSession()`)
- Modify: `R/server.R` (guarded call in the websocket handler, ~line 176)
- Test: `tests/testthat/test-mcp-session.R`

**Interfaces:**
- Produces: `mcpAnnounceSession(session)` — registers a one-shot
  `session$onFlushed()` that sends a `shiny.mcp.updateModelContext` message
  carrying `{ session: <token>, state: "connected" }` plus a text instruction
  telling the model to call `update_<appId>_app` with that `session`.
- Consumes: `session$onFlushed()`, `session$sendCustomMessage()`,
  `mcpUpdateToolName()`, `session$token`.

- [ ] **Step 1: Write the failing test**

Add to `tests/testthat/test-mcp-session.R`:

```r
test_that("MCP sessions announce their token to the model on connect", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(appId = "clock", arguments = list(
    label = ellmer::type_string("heading")
  ))
  token <- NULL
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      token <<- session$token
    }
  )
  frames <- paste(mcp_drain_frames(sess), collapse = "\n")
  expect_match(frames, "shiny.mcp.updateModelContext", fixed = TRUE)
  expect_match(frames, "update_clock_app", fixed = TRUE)
  expect_match(frames, "connected", fixed = TRUE)
  expect_match(frames, token, fixed = TRUE)

  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("non-MCP and no-arguments sessions do not announce", {
  # No mcpConfigure(arguments=...) => no announcement.
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(appId = "clock")  # enabled, but no arguments
  sess <- mcp_start_session(fluidPage(), function(input, output, session) {})
  frames <- paste(mcp_drain_frames(sess), collapse = "\n")
  expect_no_match(frames, "shiny.mcp.updateModelContext", fixed = TRUE)
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: FAIL — no `shiny.mcp.updateModelContext` frame on connect.

- [ ] **Step 3: Implement `mcpAnnounceSession()`**

In `R/mcp-session.R`, add (near `mcpUpdateModelContext()`):

```r
# Tell the model, once per MCP session, the id it must pass to
# update_<appId>_app to change THIS running instance in place. Sent after the
# first flush (the client bridge is initialized by then) via the same
# updateModelContext bridge channel. The token instruction is text so the
# model knows to echo the id; the structured {session, state} carries it
# machine-readably. Only meaningful when an update_* tool exists (arguments
# declared) and this is an MCP session.
mcpAnnounceSession <- function(session) {
  session$onFlushed(
    function() {
      token <- session$token
      instruction <- sprintf(
        paste(
          "This app instance's id is \"%s\". To change THIS instance in",
          "place, call %s with session = \"%s\". Do not re-open the app to",
          "change it."
        ),
        token, mcpUpdateToolName(), token
      )
      session$sendCustomMessage("shiny.mcp.updateModelContext", list(
        content = list(list(type = "text", text = instruction)),
        structuredContent = list(session = token, state = "connected")
      ))
    },
    once = TRUE
  )
}
```

- [ ] **Step 4: Trigger it for MCP sessions**

In `R/server.R`, immediately after `appsByToken$set(shinysession$token,
shinysession)` (line 176):

```r
    appsByToken$set(shinysession$token, shinysession)
    if (mcpEnabled() && mcpHasUpdateTool() && isMcpSession(shinysession)) {
      mcpAnnounceSession(shinysession)
    }
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `R -q -e 'devtools::test(filter = "mcp-session")'`
Expected: PASS. Also run the full MCP suite to catch regressions:
`R -q -e 'devtools::test(filter = "mcp-")'` — all green.

- [ ] **Step 6: Commit**

```bash
git add R/mcp-session.R R/server.R tests/testthat/test-mcp-session.R
git commit -m "feat(mcp): announce session token to the model on connect (#4415)"
```

---

## Task 6: Docs, demo apps, NEWS, and mirror

**Files:**
- Modify: `R/mcp-session.R` (roxygen: document the update tool + announcement)
- Modify: `mcp/demo-app3/app.R`, `mcp/demo-app/app.R`, `mcp/demo-app2/app.R`
  (comments only; verify zero-code-change works)
- Modify: `mcp/limitations.md`, `mcp/design-mcpConfigure.md`
- Modify: `NEWS.md`

**Interfaces:** none (documentation + demo comments).

- [ ] **Step 1: Verify the demo apps update in place with no code change**

Run demo-app3 (the clock — the issue's motivating example) and confirm the
`update_clock_app` tool changes `label` in place. Manual check with the MCP
host script:

```bash
# From repo root, with an MCP host / inspector pointed at the app's /mcp:
R -q -e 'shiny::runApp("mcp/demo-app3", port = 8000)'
# In the host: open_clock_app, then update_clock_app(session="<id>", label="Meeting clock")
# Expected: the SAME clock instance's heading changes; no new instance renders.
```

Confirm the existing `observe({ args <- mcpUpdates(); ... })` in
`mcp/demo-app3/app.R` handles the push unchanged. Add a short comment above
that observer:

```r
    # Restore args on open AND apply later in-place updates: the model can call
    # update_clock_app(session = ..., label = ...) to change this running
    # instance without re-opening it. Both arrive through mcpUpdates().
```

Add the analogous one-line comment to the `mcpUpdates()` observers in
`mcp/demo-app/app.R` and `mcp/demo-app2/app.R`.

- [ ] **Step 2: Document in `?mcp-session` roxygen**

In `R/mcp-session.R`, extend the `mcpUpdates()` bullet and the
`mcpUpdateModelContext()` bullet to mention: (a) the auto-registered
`update_<appId>_app` tool pushes server-side updates through the same
`mcpUpdates()` channel; (b) the app announces its `session` id to the model on
connect so the model can target the running instance. Regenerate docs:

```bash
R -q -e 'devtools::document()'
```

- [ ] **Step 3: Document the update tool + limitations**

In `mcp/design-mcpConfigure.md`, add a section describing `update_<appId>_app`
(auto-registered when `arguments` declared, required `session`, pushes through
`mcpUpdates()`, connect-time token announcement).

In `mcp/limitations.md`, add the two verification items from the spec:
- **Host-passthrough spike:** confirm a real host (claude.ai) forwards the
  `structuredContent` `{ session, state }` + instruction text into the model's
  readable context so it can echo the id into `update_*(session=...)`.
- **Multi-instance caveat:** with several open instances the model sees several
  tokens and must pick the right one; the model targets/loops per handle.

- [ ] **Step 4: Add a NEWS entry**

In `NEWS.md`, under the current development version, add:

```markdown
* Shiny MCP Apps configured with `mcpConfigure(arguments = ...)` now
  auto-register a companion `update_<appId>_app` tool. A model can call it to
  update an already-open app instance in place (via `mcpUpdates()`) instead of
  re-opening the app, targeting a specific instance by the `session` id the app
  announces on connect. (#4415)
```

- [ ] **Step 5: Verify docs build and mirror is consistent**

```bash
R -q -e 'devtools::document()'
R -q -e 'devtools::test(filter = "mcp-")'
```

Expected: docs regenerate cleanly; all MCP tests pass. Confirm the `mcp/`
docs are the canonical copies (per repo convention).

- [ ] **Step 6: Commit**

```bash
git add R/mcp-session.R man/ NEWS.md mcp/
git commit -m "docs(mcp): document update_<appId>_app + update demos (#4415)"
```

---

## Self-Review Notes

- **Spec coverage:** Decision 1 (explicit handle, required) → Task 1 schema +
  Task 3 handler. Decision 2 (server reactiveVal overlay) → Task 2. Decision 3
  (registry keyed by token; unknown → error) → Task 3 (reuses `appsByToken`).
  Decision 4 (auto tool, only when args, push in session domain) → Tasks 1 & 3.
  Decision 5 (reserved-key envelope + connect emit) → Tasks 4 & 5. Limitations
  + demos → Task 6.
- **Registry note:** the spec's "flat global registry keyed by `session$token`"
  is satisfied by the existing `appsByToken` (populated on session start,
  removed on close) rather than a parallel map; the per-session server-push
  `reactiveVal` is the only new per-session state (`mcpSessionUpdates`).
- **Type consistency:** `mcpUpdateToolName()`, `mcpHasUpdateTool()`,
  `mcpUpdateToolInfo()`, `mcpServerUpdatesFor()`, `mcpUpdateAppCall()`,
  `mcpAnnounceSession()` are used with the same signatures across tasks.
- **Test note (Task 2, Step 1):** the server push is exercised by setting the
  reactiveVal inside `withReactiveDomain(session, ...)` followed by
  `session$requestFlush()` — the same sequence `mcpUpdateAppCall()` uses in
  Task 3.
