# Hard-Disconnect: Per-Call Trigger (Plan A) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `session$close(hard = TRUE, message = ...)` as a documented public primitive that performs a complete session teardown in three coordinated tiers (in-process cleanup, `4001` close code as hosting-layer signal, distinct client-side closed state) — plus the `hardDisconnectMessage` argument on `shinyApp()` as the default closed-overlay text. Idle-timeout (Plan B) builds on this and ships on a separate branch later.

**Architecture:**

- App-level: new `hardDisconnectMessage` argument on `shinyApp()` is plumbed through `appOptions` → `getShinyOption()` → `ShinySession$initialize` → `private$hardDisconnectMessage`.
- Server-side per-call: `session$close(hard, message)` sets `private$wasHardClose`, sends a `hardDisconnectConfig` custom message inline, and closes the websocket with code `4001`/reason `"shiny-hard-disconnect"`.
- Server-side teardown: `wsClosed()` reads `private$wasHardClose`. If true, it additionally clears the root-level `inputs`, `clientData`, `downloads`, `files` Maps that the existing `invokeDestroyCallbacks` flow skips. The existing `appsByToken$remove` in `R/server.R` already handles registry cleanup symmetrically.
- Client-side: a new `hardDisconnectConfig` message handler stashes the author's message; the `socket.onclose` handler short-circuits on code `4001` into `$enterClosedState()`, which renders a distinct `#shiny-closed-overlay`, fires `shiny:closed`, posts `closed` to the parent window, and disposes the action queue / scheduled reconnect.
- Documentation: `session$close()` is promoted from an effectively-internal method to a documented member of the public session API.

**Tech Stack:** R6 / shiny package, TypeScript (`srcts/src/shiny/shinyapp.ts`), SCSS (`srcts/src/scss/`), testthat for R tests, mocha-equivalent harness for `srcts/` (Vitest based on `package.json`).

**Reference documents:**
- Spec: `docs/superpowers/specs/2026-06-03-hard-disconnect-design.md`
- Worktree branch: `feat/hard-disconnect`

**Out of scope for this plan:** idle timeout (`hardDisconnectAfter`), the `lastClientActivity` timestamp, and the `later::later` polling tick — all of those go in Plan B on a follow-up branch.

---

## Task 1: Test scaffold — FakeWebSocket helper

A reusable test fixture that records what `private$websocket$send(...)` and `private$websocket$close(...)` receive. Needed by every R test in this plan that exercises `close()` / `wsClosed()`.

**Files:**
- Create: `tests/testthat/helper-hard-disconnect.R`

- [ ] **Step 1: Write the helper**

```r
# tests/testthat/helper-hard-disconnect.R
#
# FakeWebSocket: records sent JSON payloads and close calls for assertions.
# Implements the subset of the httpuv websocket interface that ShinySession
# uses via private$websocket.
FakeWebSocket <- R6::R6Class(
  "FakeWebSocket",
  public = list(
    sent = NULL,        # character vector of JSON payloads passed to send()
    closeCode = NULL,   # last numeric code passed to close()
    closeReason = NULL, # last character reason passed to close()
    closeCalled = FALSE,
    request = NULL,     # minimal stub so ShinySession init doesn't choke
    initialize = function() {
      self$sent <- character(0)
      self$request <- list(HTTP_GUID = NULL)
    },
    send = function(payload) {
      self$sent <- c(self$sent, as.character(payload))
      invisible()
    },
    close = function(code = NULL, reason = NULL) {
      self$closeCalled <- TRUE
      self$closeCode <- code
      self$closeReason <- reason
      invisible()
    },
    onMessage = function(fn) invisible(),
    onClose = function(fn) invisible()
  )
)

# Decode the most recently sent JSON payload into an R list.
fake_ws_last_message <- function(ws) {
  if (length(ws$sent) == 0L) return(NULL)
  jsonlite::fromJSON(ws$sent[length(ws$sent)], simplifyVector = FALSE)
}

# Return all sent payloads that match a top-level key (e.g., "custom").
fake_ws_messages_with <- function(ws, key) {
  parsed <- lapply(ws$sent, function(p) jsonlite::fromJSON(p, simplifyVector = FALSE))
  Filter(function(m) !is.null(m[[key]]), parsed)
}
```

- [ ] **Step 2: Sanity-check the helper compiles**

Run: `Rscript -e 'devtools::load_all(); source("tests/testthat/helper-hard-disconnect.R"); ws <- FakeWebSocket$new(); ws$send("{}"); ws$close(1000, "ok"); stopifnot(ws$closeCalled, ws$closeCode == 1000)'`
Expected: exits 0 with no output.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/helper-hard-disconnect.R
git commit -m "test: FakeWebSocket helper for hard-disconnect tests"
```

---

## Task 2: `hardDisconnectMessage` argument on `shinyApp()`

Plumb a new optional argument through `shinyApp()` so it lands in `appOptions`, which `applyCapturedAppOptions()` converts into a `shinyOption`, which `ShinySession$initialize()` reads via `getShinyOption()`.

**Files:**
- Modify: `R/shinyapp.R:74-108` (function signature and appOptions construction)
- Test: `tests/testthat/test-hard-disconnect.R` (new file)

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-hard-disconnect.R

test_that("shinyApp() captures hardDisconnectMessage into appOptions", {
  app <- shinyApp(
    ui = shiny::tagList(),
    server = function(input, output, session) {},
    hardDisconnectMessage = "Thanks for using the app."
  )
  expect_identical(
    app$appOptions$hardDisconnectMessage,
    "Thanks for using the app."
  )
})

test_that("shinyApp() defaults hardDisconnectMessage to NULL", {
  app <- shinyApp(
    ui = shiny::tagList(),
    server = function(input, output, session) {}
  )
  expect_null(app$appOptions$hardDisconnectMessage)
})
```

- [ ] **Step 2: Run the test, expect failure**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: both tests fail with "unused argument (hardDisconnectMessage = ...)" for the first and `app$appOptions$hardDisconnectMessage` returning a non-NULL value (or test passes vacuously for the second — that's fine).

- [ ] **Step 3: Add the argument and plumb it through**

In `R/shinyapp.R`, change the function signature:

```r
shinyApp <- function(ui, server, onStart=NULL, options=list(),
                     uiPattern="/", enableBookmarking=NULL,
                     hardDisconnectMessage = NULL) {
```

And after `appOptions <- captureAppOptions()` (around line 96), add:

```r
  # Carry hard-disconnect settings into appOptions so they become shinyOptions
  # via applyCapturedAppOptions() when the app starts.
  appOptions$hardDisconnectMessage <- hardDisconnectMessage
```

- [ ] **Step 4: Run the test, expect pass**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: both tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/shinyapp.R tests/testthat/test-hard-disconnect.R
git commit -m "feat: hardDisconnectMessage argument on shinyApp()"
```

---

## Task 3: `ShinySession` reads `hardDisconnectMessage` and initializes `wasHardClose`

Add the private fields and read the message at construction time.

**Files:**
- Modify: `R/shiny.R` — `ShinySession` private list (around line 455-492) and `initialize` (around line 913-)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-hard-disconnect.R`:

```r
test_that("ShinySession reads hardDisconnectMessage from shinyOptions at init", {
  withr::with_options(list(), {
    shiny::shinyOptions(hardDisconnectMessage = "Bye!")
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)
    expect_identical(
      session$.__enclos_env__$private$hardDisconnectMessage,
      "Bye!"
    )
    expect_false(session$.__enclos_env__$private$wasHardClose)
  })
})

test_that("ShinySession defaults hardDisconnectMessage to NULL", {
  withr::with_options(list(), {
    # Clear any leftover option from a previous test
    shiny::shinyOptions(hardDisconnectMessage = NULL)
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)
    expect_null(session$.__enclos_env__$private$hardDisconnectMessage)
  })
})
```

- [ ] **Step 2: Run the test, expect failure**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: failure — `private$hardDisconnectMessage` doesn't exist yet.

- [ ] **Step 3: Add the private fields**

In `R/shiny.R`, inside `ShinySession`'s `private = list(...)` (find a clean insertion point near the other small private fields, e.g., after the bookmarking fields around line 484):

```r
    hardDisconnectMessage = NULL,  # character(1) or NULL; default closed-overlay text
    wasHardClose = FALSE,          # set by close(hard = TRUE) so wsClosed knows
```

- [ ] **Step 4: Read the option in `initialize`**

In `R/shiny.R`, inside `initialize = function(websocket) { ... }` (around line 913), after `private$testMode <- getShinyOption("testmode", default = FALSE)` (around line 956), add:

```r
      private$hardDisconnectMessage <- getShinyOption("hardDisconnectMessage", default = NULL)
      private$wasHardClose <- FALSE
```

- [ ] **Step 5: Run the test, expect pass**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: all tests pass.

- [ ] **Step 6: Commit**

```bash
git add R/shiny.R tests/testthat/test-hard-disconnect.R
git commit -m "feat: ShinySession reads hardDisconnectMessage option at init"
```

---

## Task 4: `session$close(hard, message)` — close-code path

Extend `close()` to accept `hard` and `message` arguments. When `hard = TRUE`: send `hardDisconnectConfig`, set `wasHardClose`, close with code 4001.

**Files:**
- Modify: `R/shiny.R:1332-1336` (`close = function() { ... }`)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-hard-disconnect.R`:

```r
test_that("session$close() with no args performs a soft close (backward compatible)", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$close()

  expect_true(ws$closeCalled)
  expect_null(ws$closeCode)
  expect_null(ws$closeReason)
  expect_false(session$.__enclos_env__$private$wasHardClose)
  # No hardDisconnectConfig should have been sent
  expect_length(fake_ws_messages_with(ws, "custom"), 0L)
})

test_that("session$close(hard = TRUE) sends hardDisconnectConfig and uses code 4001", {
  withr::with_options(list(), {
    shiny::shinyOptions(hardDisconnectMessage = NULL)
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)

    session$close(hard = TRUE, message = "All done.")

    # hardDisconnectConfig sent before close
    customs <- fake_ws_messages_with(ws, "custom")
    expect_length(customs, 1L)
    expect_identical(
      customs[[1L]]$custom$hardDisconnectConfig$message,
      "All done."
    )

    # Close code 4001 with reason
    expect_true(ws$closeCalled)
    expect_identical(ws$closeCode, 4001L)
    expect_identical(ws$closeReason, "shiny-hard-disconnect")
    expect_true(session$.__enclos_env__$private$wasHardClose)
  })
})

test_that("session$close(hard = TRUE) falls back to hardDisconnectMessage when message is NULL", {
  withr::with_options(list(), {
    shiny::shinyOptions(hardDisconnectMessage = "App default text")
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)

    session$close(hard = TRUE)

    customs <- fake_ws_messages_with(ws, "custom")
    expect_identical(
      customs[[1L]]$custom$hardDisconnectConfig$message,
      "App default text"
    )
  })
})

test_that("session$close(hard = TRUE) uses framework default when no message is set anywhere", {
  withr::with_options(list(), {
    shiny::shinyOptions(hardDisconnectMessage = NULL)
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)

    session$close(hard = TRUE)

    customs <- fake_ws_messages_with(ws, "custom")
    expect_identical(
      customs[[1L]]$custom$hardDisconnectConfig$message,
      "This app has closed."
    )
  })
})
```

- [ ] **Step 2: Run the tests, expect failure**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: the `hard = TRUE` tests fail because `close()` ignores the argument and the websocket records no custom message and no code.

- [ ] **Step 3: Implement `close(hard, message)`**

Replace `R/shiny.R:1332-1336`:

```r
    close = function(hard = FALSE, message = NULL) {
      if (self$closed) return(invisible())

      if (isTRUE(hard)) {
        private$wasHardClose <- TRUE
        effectiveMessage <- message %||%
          private$hardDisconnectMessage %||%
          "This app has closed."
        # Send the closed-overlay text via the existing custom-message channel
        # before closing the socket. WebSocket message ordering is FIFO so the
        # client receives this and stashes the text before the close arrives.
        private$sendMessage(custom = list(
          hardDisconnectConfig = list(message = effectiveMessage)
        ))
        private$websocket$close(code = 4001L, reason = "shiny-hard-disconnect")
      } else {
        private$websocket$close()
      }
      invisible()
    },
```

Note: `%||%` is rlang's null-coalescing operator, already available throughout the shiny package.

- [ ] **Step 4: Run the tests, expect pass**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: all four tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/shiny.R tests/testthat/test-hard-disconnect.R
git commit -m "feat: session\$close(hard, message) with 4001 close code"
```

---

## Task 5: `wsClosed()` performs full teardown when `wasHardClose`

When the websocket closes after a hard `close()`, also clear the root-level `inputs`, `clientData`, `downloads`, and `files` Maps. The existing `invokeDestroyCallbacks("")` already destroys all module scopes and outputs; we extend the root case to also include those four collections.

**Files:**
- Modify: `R/shiny.R:1337-1349` (`wsClosed = function() { ... }`)

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-hard-disconnect.R`:

```r
test_that("wsClosed after soft close leaves root Maps in place", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  # Populate Maps with sentinel entries so we can detect cleanup
  session$files$set("file1", list())
  session$downloads$set("dl1", list())

  session$close()
  session$wsClosed()

  expect_true(session$files$containsKey("file1"))
  expect_true(session$downloads$containsKey("dl1"))
})

test_that("wsClosed after hard close clears root inputs, clientData, downloads, files", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$files$set("file1", list())
  session$downloads$set("dl1", list())

  session$close(hard = TRUE, message = "bye")
  session$wsClosed()

  expect_false(session$files$containsKey("file1"))
  expect_false(session$downloads$containsKey("dl1"))
  expect_length(session$.__enclos_env__$private$.input$names(), 0L)
  expect_length(session$.__enclos_env__$private$.clientData$names(), 0L)
})

test_that("onSessionEnded runs before hard cleanup (callback sees live state)", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$files$set("sentinel", list(payload = "still here"))

  observedFiles <- NULL
  session$onSessionEnded(function() {
    # At the moment this fires, files should still be populated.
    observedFiles <<- session$files$keys()
  })

  session$close(hard = TRUE)
  session$wsClosed()

  expect_true("sentinel" %in% observedFiles)
  # And after wsClosed completes, the file is cleaned up.
  expect_false(session$files$containsKey("sentinel"))
})
```

- [ ] **Step 2: Run the tests, expect failure**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: the hard-close test fails because `wsClosed()` doesn't currently clear those collections.

- [ ] **Step 3: Extend `wsClosed()`**

Replace `R/shiny.R:1337-1349`:

```r
    wsClosed = function() {
      self$closed <- TRUE
      for (output in private$.outputs) {
        output$suspend()
      }
      # ..stacktraceon matches with the top-level ..stacktraceoff..
      withReactiveDomain(self, {
        otel_span_session_end(domain = self, {
          private$closedCallbacks$invoke(onError = printError, ..stacktraceon = TRUE)
        })
      })
      private$invokeDestroyCallbacks("")
      if (isTRUE(private$wasHardClose)) {
        # Hard close: also clear the root-level Maps that the destroy walk
        # intentionally skips for the soft path.
        private$.input$destroyAll()
        private$.clientData$destroyAll()
        self$files$reset()
        self$downloads$reset()
      }
    },
```

Note: `destroyAll()` on `ReactiveValues` and `reset()` on `Map` are the existing methods used elsewhere — verify both exist before relying on them. Search reactivity.R and map.R if either is missing, and substitute the equivalent (e.g., iterate keys and remove). See "If `destroyAll` doesn't exist" note at the end of this task.

- [ ] **Step 4: Run the tests, expect pass**

Run: `Rscript -e 'devtools::test(filter = "hard-disconnect")'`
Expected: all hard-disconnect tests pass; the soft-close test continues to pass; no other test failures.

- [ ] **Step 5: Run the broader test suite to check for regressions**

Run: `Rscript -e 'devtools::test()'`
Expected: no new failures relative to the baseline before this task. If any test breaks, investigate before continuing.

- [ ] **Step 6: Commit**

```bash
git add R/shiny.R tests/testthat/test-hard-disconnect.R
git commit -m "feat: wsClosed clears root Maps on hard close"
```

**If `destroyAll`/`reset` doesn't exist for those types:** substitute the equivalent loop, e.g.,
```r
for (k in private$.input$names()) private$.input$remove(k)
for (k in self$files$keys()) self$files$remove(k)
```
The semantics are identical; only the method names differ.

---

## Task 6: Documentation — elevate `session$close()` to documented public API

Add a `close()` entry to the session methods list in the roxygen docs, with the `hard` and `message` arguments documented and an example.

**Files:**
- Modify: `R/shiny.R` — the roxygen comment block for `session` (around lines 99-315), specifically the list items in alphabetical order.

- [ ] **Step 1: Identify the insertion point**

Run: `grep -n "item{clientData}\|item{destroy" R/shiny.R | head -5`
Confirm: `\item{close...}` doesn't exist yet, and find an alphabetically appropriate location (between `\item{allowReconnect(value)}` and `\item{clientData}`).

- [ ] **Step 2: Add the docstring entry**

After `\item{allowReconnect(value)}{ ... }` (ending around line 110), insert:

````r
#' \item{close(hard = FALSE, message = NULL)}{
#'   Closes the session. By default, performs a soft close: the websocket is
#'   shut down and Shiny tears down output observers and reactive scopes, but
#'   leaves the root `inputs`, `clientData`, `downloads`, and `files`
#'   collections in place. When `hard = TRUE`, performs a complete teardown:
#'   sends a closed-overlay message to the browser, closes the websocket with
#'   application close code `4001` (which hosting platforms can recognize as
#'   "do not hold this worker for reconnect"), and additionally clears the
#'   four root-level collections.
#'
#'   `message` overrides the default closed-overlay text for this call; when
#'   `NULL` it falls back to `shinyApp(hardDisconnectMessage = ...)`, then to
#'   `"This app has closed."`. Only meaningful when `hard = TRUE`.
#'
#'   Typical hard-close pattern, paired with a confirmation modal:
#'
#'   ```
#'   observeEvent(input$submit, {
#'     showModal(modalDialog(
#'       "Your responses have been recorded. This app will close shortly.",
#'       footer = NULL, easyClose = FALSE
#'     ))
#'     later::later(
#'       ~ session$close(hard = TRUE, message = "Thanks!"),
#'       delay = 3
#'     )
#'   })
#'   ```
#'
#'   See also `onSessionEnded` (callbacks run before close completes) and
#'   `destroy` (module-scope teardown without ending the session).
#' }
````

- [ ] **Step 3: Verify roxygen output regenerates without error**

Run: `Rscript -e 'devtools::document()'`
Expected: regenerates `man/session.Rd` (or the relevant page) with no warnings about malformed roxygen.

- [ ] **Step 4: Check the rendered docs**

Run: `Rscript -e 'devtools::load_all(); ?session'` (or inspect `man/session.Rd` directly)
Expected: the `close()` entry appears with the arguments and example.

- [ ] **Step 5: Commit**

```bash
git add R/shiny.R man/
git commit -m "docs: document session\$close() in session help"
```

---

## Task 7: Client TS — `hardDisconnectConfig` custom message handler

Stash the author's text on the `ShinyApp` instance when the message arrives.
The wire format is `{ "custom": { "hardDisconnectConfig": { "message": "..." } } }`
(the server sends it via `private$sendMessage(custom = list(hardDisconnectConfig = ...))`),
so the handler is registered through `addCustomMessageHandler`, which is what dispatches
inner keys of the `custom` envelope. The plain `addMessageHandler` would be wrong here
because the message arrives nested inside `custom`, not at the top level.

**Files:**
- Modify: `srcts/src/shiny/shinyapp.ts` — class member fields (around line 155-159) and `_init()` handlers.

- [ ] **Step 1: Add the field**

In `srcts/src/shiny/shinyapp.ts`, near the existing `$allowReconnect` field around line 158:

```ts
  $allowReconnect: boolean | "force" = false;
  $hardDisconnectMessage: string | null = null;
```

- [ ] **Step 2: Register the custom-message handler**

In `srcts/src/shiny/shinyapp.ts`, in `_init()`, after the `addMessageHandler("custom", ...)` block (around line 854-867), register the `hardDisconnectConfig` sub-handler. Look for an existing spot where built-in custom handlers are registered (search for `addCustomMessageHandler(` in this file). If none exists, register it in `_init()` immediately before the `addMessageHandler("custom", ...)` block, since handlers must be registered before the first message arrives:

```ts
    addCustomMessageHandler(
      "hardDisconnectConfig",
      (message: { message: string }) => {
        this.$hardDisconnectMessage = message.message;
      },
    );
```

Note: `addCustomMessageHandler` is defined at the top of `shinyapp.ts` (around line 91) — it's the same function exported to `window.Shiny.addCustomMessageHandler`. Internal code can call it directly.

- [ ] **Step 3: Run the TypeScript build to verify it compiles**

Run: `npm run build`
Expected: clean build, no type errors.

- [ ] **Step 4: Commit**

```bash
git add srcts/src/shiny/shinyapp.ts
git commit -m "feat(client): stash hardDisconnectConfig message"
```

---

## Task 8: Client TS — `socket.onclose` recognizes code 4001 → enter closed state

Add the early-return on `4001` before the existing reconnect logic, and implement `$enterClosedState()`.

**Files:**
- Modify: `srcts/src/shiny/shinyapp.ts` — the `socket.onclose` handler (around line 262-278) and add a new private method `$enterClosedState()`.

- [ ] **Step 1: Modify `socket.onclose`**

In `srcts/src/shiny/shinyapp.ts`, around line 262, replace:

```ts
    socket.onclose = (e) => {
      const restarting = e.code === 1012; // Uvicorn sets this code when autoreloading
      // These things are needed only if we've successfully opened the
      // websocket.
      if (hasOpened) {
        $(document).trigger({
          type: "shiny:disconnected",
          // @ts-expect-error; Can not remove info on a established, malformed Event object
          socket: socket,
        });

        this.$notifyDisconnected();
      }

      this.onDisconnected(restarting); // Must be run before this.$removeSocket()
      this.$removeSocket();
    };
```

with:

```ts
    socket.onclose = (e) => {
      // Hard-disconnect close code from the server: short-circuit the
      // reconnect logic and enter the closed state.
      if (e.code === 4001) {
        if (hasOpened) {
          $(document).trigger({
            type: "shiny:disconnected",
            // @ts-expect-error; Can not remove info on a established, malformed Event object
            socket: socket,
          });
        }
        this.$enterClosedState();
        this.$removeSocket();
        return;
      }

      const restarting = e.code === 1012; // Uvicorn sets this code when autoreloading
      if (hasOpened) {
        $(document).trigger({
          type: "shiny:disconnected",
          // @ts-expect-error; Can not remove info on a established, malformed Event object
          socket: socket,
        });

        this.$notifyDisconnected();
      }

      this.onDisconnected(restarting); // Must be run before this.$removeSocket()
      this.$removeSocket();
    };
```

- [ ] **Step 2: Implement `$enterClosedState()`**

Add a new method on the `ShinyApp` class, near `onDisconnected()` (around line 352):

```ts
  $enterClosedState(): void {
    const message =
      this.$hardDisconnectMessage ?? "This app has closed.";

    // Render the closed-state overlay (distinct from #shiny-disconnected-overlay).
    if ($("#shiny-closed-overlay").length === 0) {
      const overlay = $('<div id="shiny-closed-overlay" role="alert" aria-live="assertive"></div>');
      overlay.text(message);
      $(document.body).append(overlay);
    }
    // Remove the disconnected overlay if it raced in.
    $("#shiny-disconnected-overlay").remove();

    // Stop any scheduled reconnect.
    clearTimeout(this.scheduledReconnect);

    // Notify the page and any iframe parent that the session is closed
    // (distinct from the existing "disconnected" notification).
    $(document).trigger({
      type: "shiny:closed",
      // @ts-expect-error; we are deliberately stuffing data onto Event
      detail: { message },
    });
    if (window.parent) {
      window.parent.postMessage("closed", "*");
    }
  }
```

- [ ] **Step 3: Run the TypeScript build**

Run: `npm run build`
Expected: clean build, no type errors.

- [ ] **Step 4: Hand-test in a local app**

Create a one-off scratch app to verify end-to-end:

```bash
cat > /tmp/hard-disconnect-test/app.R <<'EOF'
library(shiny)
shinyApp(
  ui = fluidPage(
    actionButton("close", "Close hard"),
    p("If this app is in the closed state, the button does nothing.")
  ),
  server = function(input, output, session) {
    observeEvent(input$close, {
      session$close(hard = TRUE, message = "Thanks, that's it.")
    })
  },
  hardDisconnectMessage = "Closed (default)."
)
EOF
Rscript -e 'shiny::runApp("/tmp/hard-disconnect-test", launch.browser = TRUE, port = 3838)'
```

In the browser:
1. Click the button.
2. Observe: a distinct overlay appears with "Thanks, that's it." (not the grey disconnected overlay).
3. No reconnect dialog appears.
4. The page becomes inert.

If the overlay is unstyled / invisible, that's expected — styling lands in Task 9.

- [ ] **Step 5: Commit**

```bash
git add srcts/src/shiny/shinyapp.ts
git commit -m "feat(client): \$enterClosedState on socket code 4001"
```

---

## Task 9: SCSS for `#shiny-closed-overlay`

Style the closed overlay so it's visually distinct from the disconnected overlay (which is a grey haze used for transient network drops). The closed overlay should communicate finality — e.g., centered card with the message.

**Files:**
- Find: existing styles for `#shiny-disconnected-overlay` in `srcts/src/scss/` to use as a starting point.
- Modify or create: `srcts/src/scss/_closed-overlay.scss` (or add to the same file that styles the disconnected overlay).

- [ ] **Step 1: Locate the existing disconnected-overlay styles**

Run: `grep -rn "shiny-disconnected-overlay" srcts/src/scss/`
Expected: one or two files. Open them to confirm the pattern used.

- [ ] **Step 2: Add closed-overlay styles**

In the same SCSS file as `#shiny-disconnected-overlay`, append:

```scss
#shiny-closed-overlay {
  position: fixed;
  inset: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  background-color: rgba(255, 255, 255, 0.92);
  color: #222;
  font-size: 1.125rem;
  line-height: 1.5;
  padding: 2rem;
  text-align: center;
  z-index: 99999;
  // Distinguish from the disconnected overlay (grey-out) — fully opaque card,
  // not a transient haze.
  pointer-events: auto;
}
```

If the existing scss already imports a sibling file pattern (e.g., `@import "disconnected-overlay";`), add a parallel import for the closed overlay file instead.

- [ ] **Step 3: Rebuild CSS**

Run: `npm run build`
Expected: clean build.

- [ ] **Step 4: Hand-test in a browser**

Re-run the scratch app from Task 8, click the button, and confirm the overlay is now styled (centered text, opaque background, no longer "looks like the grey disconnected screen").

- [ ] **Step 5: Commit**

```bash
git add srcts/src/scss/ inst/www/ srcts/types/
git commit -m "feat(client): style #shiny-closed-overlay"
```

(Adjust the staged paths based on what `npm run build` regenerates — typically `inst/www/shared/shiny.css` and any TS type bundles.)

---

## Task 10: TypeScript tests for closed-state flow

Cover the JS path with a unit test: simulate a `socket.onclose` event with code 4001 and verify the DOM gets the closed overlay, the `shiny:closed` event fires, and no reconnect is scheduled.

**Files:**
- Find: existing TS tests in `srcts/` (check `package.json` for the test runner — likely vitest or jest).
- Test: an existing or new `*.test.ts` file alongside `shinyapp.ts`.

- [ ] **Step 1: Locate the test setup**

Run: `cat package.json | grep -A2 '"scripts"' ; find srcts -name "*.test.ts" -o -name "*.spec.ts" | head -10`
Expected: identifies the test command (e.g., `npm test` or `npm run test:unit`) and any existing test files to copy the setup from.

If no TS tests exist yet, write the test as a stub that documents the expected behavior, mark this task as "DOM tests deferred to integration coverage," and proceed. **Don't introduce a new testing framework just for this plan.**

- [ ] **Step 2: If TS tests exist, write the closed-state test**

Use the existing test framework's idioms. Roughly:

```ts
import { describe, it, expect, beforeEach } from "vitest";  // or the local framework
import { ShinyApp } from "./shinyapp";

describe("hard disconnect", () => {
  beforeEach(() => {
    document.body.innerHTML = "";
  });

  it("renders #shiny-closed-overlay on code 4001 and skips reconnect", () => {
    const app = new ShinyApp();
    app.$hardDisconnectMessage = "Goodbye.";
    let closedFired = false;
    document.addEventListener("shiny:closed", () => { closedFired = true; });

    app.$enterClosedState();

    const overlay = document.getElementById("shiny-closed-overlay");
    expect(overlay).not.toBeNull();
    expect(overlay!.textContent).toBe("Goodbye.");
    expect(closedFired).toBe(true);
  });

  it("falls back to default text when $hardDisconnectMessage is null", () => {
    const app = new ShinyApp();
    app.$hardDisconnectMessage = null;
    app.$enterClosedState();

    const overlay = document.getElementById("shiny-closed-overlay");
    expect(overlay!.textContent).toBe("This app has closed.");
  });
});
```

- [ ] **Step 3: Run the test**

Run: `npm test` (or the command identified in Step 1)
Expected: passes.

- [ ] **Step 4: Commit**

```bash
git add srcts/src/shiny/  # or wherever the test file lives
git commit -m "test(client): \$enterClosedState renders overlay and fires shiny:closed"
```

---

## Task 11: NEWS entry

Document the change in `NEWS.md` so it appears in the next release.

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Locate the current development version section**

Run: `head -30 NEWS.md`
Expected: a section at the top for the upcoming version (e.g., `# shiny 1.13.0.9000` or similar).

- [ ] **Step 2: Add the entry**

Under the current development version section, add a bullet:

```markdown
* `session$close()` now accepts `hard = TRUE` and `message = ...` arguments
  for performing a complete session teardown (full cleanup of root inputs /
  clientData / downloads / files, a `4001` websocket close code that hosting
  platforms can recognize as "release this worker," and a distinct closed-
  state overlay in the browser). The default closed-overlay text can be set
  app-wide via `shinyApp(hardDisconnectMessage = ...)`. (#XXXX)
```

(Replace `#XXXX` with the actual PR number once the PR is opened, or omit the parenthetical for now.)

- [ ] **Step 3: Commit**

```bash
git add NEWS.md
git commit -m "docs: NEWS entry for session\$close(hard = TRUE)"
```

---

## Task 12: Final verification

Run the full test suite and the build, then look for anything missed.

- [ ] **Step 1: Run the R test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: all tests pass. New `test-hard-disconnect.R` tests are included.

- [ ] **Step 2: Run the JS build and tests**

Run: `npm run build && npm test`
Expected: clean build, all tests pass.

- [ ] **Step 3: Run `R CMD check`**

Run: `Rscript -e 'devtools::check(args = c("--no-manual"))'`
Expected: 0 errors, 0 warnings (notes are acceptable if pre-existing on this branch). Pay particular attention to:
- Roxygen docs render without errors
- No undocumented arguments on exported functions
- No `R CMD check` complaints about the new `private$wasHardClose` field

- [ ] **Step 4: Spot-check the example from the docstring works**

Create a quick scratch app that uses the modal-then-close pattern from the new `close()` docstring. Verify it runs and produces the expected UX.

- [ ] **Step 5: Confirm the soft-close path is byte-for-byte unchanged**

Run the existing `tests/testthat/test-app.R` and any tests that exercise `session$close()`:
```bash
Rscript -e 'devtools::test(filter = "session|destroy|app")'
```
Expected: no regressions.

- [ ] **Step 6: Commit any final touch-ups, then prepare the PR**

If any cleanups landed, commit them. Then summarize the branch:

```bash
git log --oneline main..HEAD
git diff --stat main..HEAD
```

Review the commit history to make sure each commit is focused, and squash/reorder as needed before opening the PR.

---

## Spec coverage check

Quick map from spec sections to tasks:

| Spec section | Task(s) |
|---|---|
| R API — `hardDisconnectMessage` arg | 2 |
| R API — `session$close(hard, message)` | 4 |
| R API — Documentation elevation | 6 |
| Architecture — `wsClosed` hard path | 5 |
| Architecture — `close(hard, message)` flow | 4 |
| Wire protocol — runtime config | 4 (server side) + 7 (client side) |
| Wire protocol — close code 4001 | 4 (server side) + 8 (client side) |
| Client behavior — `$enterClosedState`, overlay, event | 8, 9, 10 |
| Testing — R unit + soft-unchanged | 4, 5 |
| Testing — TS DOM | 10 |
| NEWS / docs | 6, 11 |
| Final verification | 12 |

Out of scope (Plan B): idle timeout, `hardDisconnectAfter`, `lastClientActivity`, polling tick.

Out of scope (separate work item): hosting platform changes in shiny-server / Connect. The spec's hosting handoff brief can go out to those teams in parallel with this work; nothing in this plan depends on platform changes.
