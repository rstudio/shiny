# Extended tasks: keep the invoking session responsive

## Overview

`ExtendedTask` runs a slow operation in the background while the *session
that started it* stays fully interactive — the user can keep clicking and
submitting other inputs while the work finishes. Do NOT run slow work (an
API call, a big query, model inference) directly inside a reactive
expression, observer, or output: that blocks the whole session until it
returns, freezing the UI for that one user. (An async promise returned from
a render function only unblocks *other* sessions — see the async topic —
the session that kicked off the work still waits.)

## Create a task

`ExtendedTask$new(func)` wraps a function that returns something
`promises::as.promise()` understands. Reach for `mirai::mirai()`: it hands
the expression to a background R process, so the heavy computation never
touches the main one. (A plain `promises::promise`,
`promises::future_promise()`, or a `future::future()` object is accepted
too.) Create the task once, near the top of `server` (or a module server
function), not inside a reactive. `func` must not read reactive inputs
directly — they may have changed by run time — so pass any values it needs
as arguments instead.

```r
library(shiny)
library(mirai)

# Persistent background processes for tasks to run in, released on exit.
daemons(2)
onStop(function() daemons(0))

ui <- fluidPage(
  numericInput("n", "Number to square", 5),
  actionButton("run", "Compute, slowly"),
  textOutput("status"),
  textOutput("result")
)

server <- function(input, output, session) {
  slow_square <- ExtendedTask$new(function(n) {
    mirai(
      {
        Sys.sleep(2) # pretend this is expensive
        n * n
      },
      n = n # values `.expr` needs must be passed in explicitly
    )
  })

  observeEvent(input$run, {
    slow_square$invoke(input$n)
  })

  output$status <- renderText(paste("Status:", slow_square$status()))

  output$result <- renderText({
    paste0("Result: ", slow_square$result())
  })
}

shinyApp(ui, server)
```

`mirai()`'s expression evaluates in a separate process with its own global
environment, so anything it references has to arrive through `...` (`n = n`
above) rather than being captured from the enclosing scope. `daemons(n)`
sets up persistent workers to reuse; without it each `mirai()` still runs
off the main process, but pays to start a fresh one every time.

## Invoke from an event

`task$invoke(...)` starts a run; it returns immediately (`NULL`) and never
blocks, so gate it behind `observeEvent()` or `bindEvent()` (as above) rather
than calling it unconditionally. If `invoke()` is called while a previous run
is still in progress, the new call is queued and starts only after the
current run finishes — a single `ExtendedTask` never runs two invocations at
once. `bslib::input_task_button()` plus `bind_task_button()` give you a
button that disables itself automatically while the task it's bound to is
running.

## Read status and result

`task$status()` is a reactive read returning `"initial"` (never invoked),
`"running"`, `"success"`, or `"error"`; use it to drive conditional UI such
as a spinner. `task$result()` is also a reactive read: on `"success"` it
returns the value from the most recent invocation, on `"error"` it
re-throws that error, and on `"initial"`/`"running"` it throws a silent
error — like `req(FALSE)` — that blanks the output or, while running, shows
a progress state. Reading either establishes a reactive dependency, so an
output calling `task$result()` re-renders once the task finishes.

## When to reach for `ExtendedTask`

Use `ExtendedTask` when the *same* session must stay interactive while slow
work runs — "click a button, keep using the app while it computes." A plain
promise from a render function or observer (the async topic) suits cases
where only *other* sessions must keep working, and the current session
waiting is fine. `invalidateLater(millis)` is different again: periodic
polling, not a single long-running background operation.

## Quick reference

| Function | Purpose |
|---|---|
| `ExtendedTask$new(func)` | Create a task; `func` returns a `mirai()` (or any promise-like object) |
| `task$invoke(...)` | Start a run (non-blocking); queues if already running |
| `task$status()` | Reactive read: `"initial"`/`"running"`/`"success"`/`"error"` |
| `task$result()` | Reactive read of the latest result; errors/blanks appropriately |

## Common mistakes

- Running slow work directly in an observer/reactive/output instead of via
  `ExtendedTask` → freezes the whole session; move the work into
  `ExtendedTask$new()` and invoke it from an event.
- Reading `input$x` inside the function passed to `ExtendedTask$new()` →
  the input may change before the background work runs; read it in the
  caller and pass it as an argument to `invoke()`.
- Referring to a local variable inside `mirai()` without passing it in →
  the expression runs in a separate process that never saw your globals;
  supply it as a named argument (`n = n`) or via `.args`.
- Calling `task$result()` inside `observeEvent()`, `eventReactive()`,
  `bindEvent()`, or `isolate()` → invalidation is ignored there; read it
  from a plain `reactive()`, `observe()`, or render function.
- Expecting a second `invoke()` to cancel or interrupt the first → it
  queues instead and runs after the current invocation finishes.
- Reaching for `invalidateLater()` polling to simulate background work →
  it re-runs a reactive on a timer and still blocks the session each run;
  use `ExtendedTask` for genuinely long-running work.
