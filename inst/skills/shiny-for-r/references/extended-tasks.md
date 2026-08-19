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
`promises::as.promise()` understands — a `promises::promise`, or an object
from {mirai} or {future}. Create it once, near the top of `server` (or a
module server function), not inside a reactive. `func` must not read
reactive inputs directly — they may have changed by run time — so pass any
values it needs as arguments instead.

```r
library(shiny)
library(promises)

ui <- fluidPage(
  numericInput("n", "Number to square", 5),
  actionButton("run", "Compute, slowly"),
  textOutput("status"),
  textOutput("result")
)

server <- function(input, output, session) {
  slow_square <- ExtendedTask$new(function(n) {
    promise(function(resolve, reject) {
      later::later(function() resolve(n * n), delay = 2)
    })
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

In real apps, `func`'s body is usually `mirai::mirai(...)` or
`future::future(...)` with `promises::future_promise()`, so the heavy
computation runs off the main R process entirely. `later::later()`-based
promises, as above, demonstrate the pattern without that extra dependency.

## Invoke from an event

`task$invoke(...)` starts a run; it returns immediately (`NULL`) and never
blocks, so gate it behind `observeEvent()` or `bindEvent()` (as above) rather
than calling it unconditionally. If `invoke()` is called while a previous run
is still in progress, the new call is queued and starts only after the
current run finishes — a single `ExtendedTask` never runs two invocations at
once.

## Read status and result

`task$status()` is a reactive read returning `"initial"` (never invoked),
`"running"`, `"success"`, or `"error"`; use it to drive conditional UI such
as a spinner. `task$result()` is also a reactive read: on `"success"` it
returns the value from the most recent invocation, on `"error"` it
re-throws that error, and on `"initial"`/`"running"` it throws a silent
error — like `req(FALSE)` — that blanks the output or, while running, shows
a progress state. Reading either establishes a reactive dependency, so an
output calling `task$result()` re-renders once the task finishes. Read both
naively from a render function, `reactive()`, or `observe()` — not from
`observeEvent()`, `eventReactive()`, `bindEvent()`, or `isolate()`, where the
invalidation is ignored.

## Pair with a task button

{bslib}'s `input_task_button(id, label)` creates a button that disables
itself and shows a busy label while work is in flight. `bind_task_button(task,
task_button_id)` links an `ExtendedTask` to that button, so its state tracks
the task automatically:

```r
# Partial snippet: inside a server function, after creating slow_square
bslib::bind_task_button(slow_square, "run")
```

Binding only syncs the button's state — it does not invoke the task; still
call `invoke()` from `observeEvent()`/`bindEvent()`.

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
| `ExtendedTask$new(func)` | Create a task; `func` returns a promise (or mirai/future) |
| `task$invoke(...)` | Start a run (non-blocking); queues if already running |
| `task$status()` | Reactive read: `"initial"`/`"running"`/`"success"`/`"error"` |
| `task$result()` | Reactive read of the latest result; errors/blanks appropriately |
| `bslib::input_task_button(id, label)` | Button that shows busy state automatically |
| `bslib::bind_task_button(task, task_button_id)` | Link a task's status to a task button |

## Common mistakes

- Running slow work directly in an observer/reactive/output instead of via
  `ExtendedTask` → freezes the whole session; move the work into
  `ExtendedTask$new()` and invoke it from an event.
- Reading `input$x` inside the function passed to `ExtendedTask$new()` →
  the input may change before the background work runs; read it in the
  caller and pass it as an argument to `invoke()`.
- Calling `task$result()` inside `observeEvent()`, `eventReactive()`,
  `bindEvent()`, or `isolate()` → invalidation is ignored there; read it
  from a plain `reactive()`, `observe()`, or render function.
- Expecting a second `invoke()` to cancel or interrupt the first → it
  queues instead and runs after the current invocation finishes.
- Reaching for `invalidateLater()` polling to simulate background work →
  it re-runs a reactive on a timer and still blocks the session each run;
  use `ExtendedTask` for genuinely long-running work.
