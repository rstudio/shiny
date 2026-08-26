# Session lifecycle in Shiny for R

## Overview

Every connected browser gets its own session, and `server` runs once per
session — the natural home for per-user state and cleanup. Do NOT store
per-user state in a variable defined outside `server` (top of `app.R`, or in
`global.R`): that code runs once per R process and is shared by every
session, so one user's writes leak into another's app. Per-user state
belongs inside `server`, typically in a `reactiveVal()` or `reactiveValues()`.

```r
# Partial snippet: scope contrast
shared_counter <- 0        # WRONG: one value shared by every session

server <- function(input, output, session) {
  my_counter <- reactiveVal(0)   # RIGHT: a fresh value per session
}
```

## Clean up when a user disconnects: `onSessionEnded()` / `session$onSessionEnded()`

Register a callback that runs after the client disconnects. Use it to close
connections, cancel background work, or delete temp files. Call it as a
free function inside `server` (it defaults to the current session), or as
`session$onSessionEnded()` on an explicit session. Both return a function
that cancels the registration.

```r
library(shiny)

ui <- fluidPage(
  p("Reload the app or close the tab to trigger cleanup."),
  verbatimTextOutput("path")
)

server <- function(input, output, session) {
  tmp <- tempfile(fileext = ".txt")
  writeLines("scratch data", tmp)

  onSessionEnded(function() {
    if (file.exists(tmp)) unlink(tmp)   # runs once, when this session ends
  })

  output$path <- renderText(tmp)
}

shinyApp(ui, server)
```

`onStop()` is the more general form: inside `server` it behaves like
`onSessionEnded()`; called outside `server` (e.g. top of `app.R`), it runs
once when the whole application (`runApp()`) exits — useful for
process-level setup/teardown alongside `global.R`.

Closing a session (`session$close()`, or the browser disconnecting) is
**not** the same as destroying it: observers are torn down, but reactive
values and expressions stay readable at their last value rather than
erroring. Background work that outlives the connection (an in-flight async
task, say) can still read those values safely after the user disconnects.
Contrast that with a module's `session$destroy(namespace)`, which
hard-destroys a specific module scope: reading from it afterward errors.

## React to errors: `onUnhandledError()`

Registers a function called when an unhandled error occurs — one your own
code didn't catch. Not all of these are equal: an error in a render
function shows as an "Error" output and the session carries on, while an
error in an observer is fatal and closes the session. The callback receives
the error condition — fatal ones carry the class `shiny.error.fatal`, which
is what the example below branches on. It cannot stop the error or keep a
fatal one from closing the session; use it only to log or clean up.

```r
library(shiny)

ui <- fluidPage(
  sliderInput("number", "Number", 0, 10, 4),
  textOutput("text")
)

server <- function(input, output, session) {
  onUnhandledError(function(err) {
    level <- if (inherits(err, "shiny.error.fatal")) "FATAL" else "ERROR"
    message(level, ": ", conditionMessage(err))
  })

  output$text <- renderText({
    if (input$number > 7) stop("that's too high!")
    sprintf("You picked number %d.", input$number)
  })
}

shinyApp(ui, server)
```

## Hook the reactive flush: `session$onFlush()` / `session$onFlushed()`

A flush is when Shiny recomputes invalidated reactives and sends the
resulting output updates to the client. `session$onFlush(fun)` runs *before*
that send; `session$onFlushed(fun)` runs *after*. Both default to `once =
TRUE`; pass `once = FALSE` to run on every flush instead of just the next.

```r
# Partial snippet: inside a server function
session$onFlushed(function() {
  message("client updated")
}, once = FALSE)
```

## Store arbitrary per-session data: `session$userData`

An environment, created fresh per session, for app or module authors to
stash session-specific data — a database handle, a counter, a cache. As a
plain environment, reading and writing it needs no reactive context.

```r
# Partial snippet: inside a server function
session$userData$db_conn <- DBI::dbConnect(RSQLite::SQLite(), "app.db")
onSessionEnded(function() {
  DBI::dbDisconnect(session$userData$db_conn)
})
```

## Quick reference

| Function | Purpose |
|---|---|
| `onSessionEnded(fun)` / `session$onSessionEnded(fun)` | Cleanup after the client disconnects |
| `onStop(fun)` | Session cleanup inside `server`; app-exit cleanup outside it |
| `onUnhandledError(fun)` | Log/react to an unhandled error (fatal ones carry `shiny.error.fatal`) |
| `session$onFlush(fun, once = TRUE)` | Run code just before output updates are sent |
| `session$onFlushed(fun, once = TRUE)` | Run code just after output updates are sent |
| `session$userData` | Per-session environment for arbitrary state |
| `session$close()` | End the session from the server side |

## Common mistakes

- Per-user state defined outside `server` (top of `app.R`, or a `global.R`
  variable) → shared by every session; move it inside `server`, typically as
  a `reactiveVal()`/`reactiveValues()`.
- Assuming a closed session erases its reactive values → it does not; they
  stay readable at their last value, only observers are torn down. Use
  `session$destroy(namespace)` in module code for a hard teardown instead.
- Doing cleanup only in `global.R` or at the bottom of `app.R` → runs once
  per process, not per user; register per-session cleanup with
  `onSessionEnded()`.
- Expecting `onUnhandledError()` to recover from the error or keep a fatal
  one from closing the session → it cannot; it is observation-only, for
  logging or cleanup around a failure already happening.
- Reading `session$userData` before `server` has a `session` argument in
  scope → it only exists inside the server function; it is not part of
  `global.R`.
