# Async rendering with promises

## Overview

Shiny render functions and observers can return a `promises::promise` in
place of a plain value, letting the R process serve other sessions while a
slow operation (an HTTP call, a database query) resolves. Do NOT expect this
to keep the *same* session responsive — a promise returned from a render
function or observer still blocks that session's own reactive flush until it
resolves; only *other* sessions keep working in the meantime. If you need
the invoking session itself to stay responsive while a task runs, that is
the extended-tasks topic, not this one.

{promises} (>= 1.5.0) is a hard Import of shiny, so its API is always
available — no extra dependency to add.

## Return a promise from a render function

Any `render*()` function or `observe()`/`observeEvent()` body may return a
promise instead of a value. Shiny waits for it to resolve before treating the
output as done.

```r
library(shiny)
library(promises)

ui <- fluidPage(
  numericInput("n", "Pick a number", 4),
  textOutput("result")
)

server <- function(input, output, session) {
  output$result <- renderText({
    n <- input$n
    promise(function(resolve, reject) {
      later::later(function() resolve(n * n), delay = 1)
    }) |> then(function(square) {
      paste0("Result: ", square)
    })
  })
}

shinyApp(ui, server)
```

`promise(function(resolve, reject) ...)` wraps any callback-based async
operation. Calling `resolve(value)` fulfills the promise; `reject(error)`
rejects it. For CPU-bound work, `mirai::mirai()` runs the expression in a
background R process and returns an object that drops in wherever a
`promise()` is expected. `promises::future_promise()` is the {future}
equivalent: prefer it over a bare `future::future()`, which blocks the main
R process once every {future} worker is busy — `future_promise()` instead
returns a promise immediately and starts the work when a worker frees up.

## Chain steps with `then()`

`promises::then(promise, onFulfilled, onRejected)` runs `onFulfilled` when
the promise resolves and returns a new promise, so steps can be chained.
Because it takes the promise first, it composes with the base pipe:

```r
# Partial snippet: inside a render function or observer
promises::promise_resolve(input$n) |>
  promises::then(function(value) value * 2) |>
  promises::then(function(value) paste("got", value))
```

Each `then()` returns a new promise, so add steps by piping again. Older
code uses the `%...>%` operator for this — recognize it when reading, but
write `then()` with `|>`, which needs no special operator and puts the
handler where every other pipeline puts it.

## Handle errors and cleanup

`promises::catch(promise, onRejected)` runs a handler only when the
upstream promise is rejected (an error was thrown or `reject()` called), and
`promises::finally(promise, onFinally)` runs regardless of success or
failure — useful for releasing a resource such as a database connection.

```r
# Partial snippet: error-handling chain in a render function or observer
promises::promise_resolve(input$n) |>
  promises::then(function(n) {
    if (n < 0) stop("n must be non-negative")
    sqrt(n)
  }) |>
  promises::catch(function(err) {
    message("computation failed: ", conditionMessage(err))
    NA
  }) |>
  promises::finally(function() {
    message("cleanup ran")
  })
```

`catch()` and `finally()` take the promise as their first argument, just
like `then()`, so the whole chain is one `|>` pipeline.

## Quick reference

| Function | Purpose |
|---|---|
| `promises::promise(fun)` | Wrap a callback-based async operation as a promise |
| `promises::promise_resolve(value)` | Create an already-fulfilled promise |
| `promises::then(p, onFulfilled, onRejected)` | Chain a step after a promise resolves; pipe with `\|>` |
| `promises::future_promise(expr)` | Run {future} work as a promise without blocking on busy workers |
| `promises::catch(p, onRejected)` | Handle a rejected promise |
| `promises::finally(p, onFinally)` | Run cleanup regardless of outcome |

## Common mistakes

- Returning a promise from an output and assuming the *current* browser tab
  stays responsive while it resolves → it does not; only other sessions
  proceed. Use an `ExtendedTask` (see the extended-tasks topic) if the
  invoking session itself needs to stay interactive.
- Forgetting `library(promises)` or the namespace prefix → `then()`,
  `catch()`, and `finally()` are not base R; they come from {promises}.
- Doing blocking I/O inside the resolve callback of `promise()` → defeats
  the purpose; only the scheduling/callback wiring should be synchronous,
  the slow work should happen off the main R process (`mirai::mirai()`, or
  `promises::future_promise()`) or in a truly async callback API.
- Letting an error inside a `then()` step propagate unhandled → attach
  `promises::catch()` to the chain so failures don't surface as a generic
  "an error has occurred" in the UI.
- Calling `future::future()` directly for the slow work → it blocks the
  main R process when {future}'s workers are all busy; wrap it with
  `promises::future_promise()`, which queues instead.
- Mixing up `then()`'s `onRejected` with `catch()` → both handle rejection,
  but `catch()` is clearer when you only care about errors and not the
  success path.
