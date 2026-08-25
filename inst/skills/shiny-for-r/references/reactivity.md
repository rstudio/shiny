# Reactivity in Shiny for R

## Overview

Shiny builds a dependency graph automatically: reading a reactive source in
a reactive context registers a dependency, so when the source changes,
everything that read it re-runs. Two habits work against that grain: doing
the same non-trivial work in several `render*()` outputs, which pays for it
once per output, and using `observe()` to push a computed value into an
output, which inverts the flow and hides the dependency. Compute once in a
`reactive()` and let outputs *pull* from it.

## Cache a derived value: `reactive()`

Wrap shared computation in `reactive()`; it memoizes the result so any
number of outputs can reuse it without repeating the work.

```r
library(shiny)

ui <- fluidPage(
  selectInput("group", "Group", choices = c("4", "6", "8")),
  verbatimTextOutput("summary"),
  verbatimTextOutput("count")
)

server <- function(input, output, session) {
  filtered <- reactive(subset(mtcars, cyl == as.numeric(input$group)))

  output$summary <- renderPrint(summary(filtered()$mpg))  # computed once
  output$count <- renderPrint(nrow(filtered()))            # reused
}

shinyApp(ui, server)
```

## Perform a side effect: `observe()` / `observeEvent()`

Use `observe()` for actions with no return value: writing a file, calling
`updateSelectInput()`, logging. `observeEvent(eventExpr, handlerExpr)`
restricts it to one trigger. `bindEvent(x, ...)` does the same but also
works on `reactive()`/`render*()`, when a value must come back out.

```r
library(shiny)

ui <- fluidPage(
  actionButton("go", "Compute"),
  textInput("name", "Name"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  log <- reactiveVal(NULL)
  observeEvent(input$go, log(paste("Hello,", input$name)))   # action

  greeting <- reactive(paste("Hi,", input$name)) |> bindEvent(input$go)  # value

  output$out <- renderPrint(list(log = log(), greeting = greeting()))
}

shinyApp(ui, server)
```

Both accept `ignoreNULL` (skip a `NULL`/`0` event) and `ignoreInit` (skip
creation-time run).

## Share a mutable value: `reactiveVal()` / `reactiveValues()`

`reactiveVal(value)` returns a function: call it with no arguments to read,
with one to write. `reactiveValues(...)` is a list-like object with multiple
named reactive slots, read and written with `$`.

```r
# Partial snippet: inside a server function
counter <- reactiveVal(0)
counter(counter() + 1)              # read current, write new

state <- reactiveValues(x = 0, y = 0)
state$x <- state$x + 1              # each field invalidates independently
```

## Short-circuit / validate: `req()`

Stops execution when a value is missing or falsy, pausing dependent outputs
until it's satisfied. `req(x, cancelOutput = TRUE)` leaves the previous
output in place instead of blanking it.

```r
library(shiny)

ui <- fluidPage(
  textInput("name", "Name", value = ""),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    req(input$name)              # wait until name is non-empty
    paste("Hello,", input$name)
  })
}

shinyApp(ui, server)
```

## Read without depending: `isolate()`

Reads a reactive source without registering a dependency — useful when an
observer should re-run for one source but only needs the *current* value of
another, and when an observer reads a value it also writes (avoiding a
self-invalidating loop).

```r
# Partial snippet: inside a server function
observe({
  input$go                        # depend on the button
  seed <- isolate(input$seed)     # read seed, but don't re-run when it changes
  set.seed(seed)
})
```

`observeEvent()`, `eventReactive()`, and `bindEvent()` already evaluate their
handler inside `isolate()`, so an extra `isolate()` in those bodies is a
no-op — reach for it in `observe()`/`reactive()`/`render*()`, which do take
dependencies on everything they read.

## Timers and streaming: `invalidateLater()`, `reactivePoll()`, `reactiveFileReader()`

`invalidateLater(millis)` invalidates the current context after roughly
`millis` ms — a timer built inside `reactive()`/`observe()`.
`reactivePoll()` re-reads a source only when a cheap `checkFunc` changes;
`reactiveFileReader()` is the same for a file's mtime.

```r
# Partial snippet: reactivePoll() signature and typical use
data <- reactivePoll(
  intervalMillis = 1000,
  session = session,
  checkFunc = function() file.info(log_path)$mtime[1],
  valueFunc = function() read.csv(log_path)
)
```

## Cache across sessions: `bindCache()`

Ordinary `reactive()` remembers only its most recent value; `bindCache()`
keeps one entry per key and, by default (`cache = "app"`), shares them across
sessions. The app cache is a bounded LRU `cachem::cache_mem()` (~200 MB by
default), so old entries are evicted rather than kept forever — treat a hit
as likely, not guaranteed. Use the reactive's own expressions as the key;
pair with `bindEvent()` to defer work until requested.

```r
# Partial snippet: inside a server function
slow_result <- reactive({
  Sys.sleep(2)                    # pretend this is expensive
  input$x * input$y
}) |>
  bindCache(input$x, input$y)
```

## Quick reference

| Function | Purpose |
|---|---|
| `reactive(expr)` | Cache a derived value |
| `observe(expr)` | Side effect, reruns on any dependency change |
| `observeEvent(eventExpr, handlerExpr)` | Side effect on one event |
| `bindEvent(x, ...)` | Restrict a reactive/observer/render to given events |
| `reactiveVal(value)` | Settable value; `v()` reads, `v(new)` writes |
| `reactiveValues(...)` | Named reactive slots, read/write with `$` |
| `req(...)` | Pause until arguments are truthy |
| `isolate(expr)` | Read without depending |
| `invalidateLater(millis)` | Invalidate on a timer |
| `reactivePoll(intervalMillis, session, checkFunc, valueFunc)` | Re-read on cheap-check change |
| `reactiveFileReader(intervalMillis, session, filePath, readFunc)` | `reactivePoll()` for a file |
| `bindCache(x, ...)` | Cache every value by key, across sessions |

## Common mistakes

- Calling a reactive without parens (`filtered` not `filtered()`) → returns
  the function itself.
- Using `observe()` to compute a displayed value → effects return nothing;
  use `reactive()` and read it from a `render*()`.
- Reading `input$x` outside a reactive context → "Operation not allowed
  without an active reactive context." Read inside `reactive()`,
  `observe()`, `render*()`, or `isolate()`.
- Missing `req()` on an empty input → errors or a spurious result; guard
  with `req(input$x)`.
- Wrapping every read in `isolate()` "to be safe" → updates stop firing;
  isolate only the reads that must not retrigger.
- Adding `isolate()` inside an `observeEvent()`/`eventReactive()` handler →
  redundant; those bodies already run isolated. Use it in `observe()`,
  `reactive()`, or a `render*()` function instead.
