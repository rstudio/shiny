# Debugging Shiny for R apps

## Overview

Shiny gives you a real R debugger inside a running reactive graph. Do NOT
sprinkle `print()` statements through a server function and rerun the app
to narrow down a bug — drop a single `browser()` at the point of failure
(or have Shiny drop into one on error) and inspect `input`, reactives, and
the call stack interactively instead.

## Break at a specific point: `browser()`

Call `browser()` anywhere inside a reactive expression, observer, or render
function to pause there with an interactive R console that can see `input`
and any reactives in scope. Reactive code only runs when Shiny decides it's
invalidated, so place `browser()` inside the block you suspect, not before
the app starts.

```r
# Partial snippet: inside a server function
output$plot <- renderPlot({
  browser()          # execution pauses here, in the render function's scope
  df <- filtered_data()
  plot(df$x, df$y)
})
```

## Break automatically on any error: `options(shiny.error = browser)`

Set this once and Shiny drops into `browser()` where an uncaught error
occurs, instead of printing it and unwinding. Unset with
`options(shiny.error = NULL)` when done.

```r
# Partial snippet: run before shinyApp(ui, server), or in the R console
options(shiny.error = browser)
```

## See full stack traces: `options(shiny.fullstacktrace = TRUE)`

By default, Shiny prints a shortened, "pretty" stack trace that hides
internal Shiny frames. Turn this on to see the complete call stack —
useful when the bug is in how your code interacts with Shiny's reactive
machinery rather than in app logic.

```r
# Partial snippet: run before reproducing the error
options(shiny.fullstacktrace = TRUE)
```

## Watch the websocket traffic: `options(shiny.trace = TRUE)`

Prints every message sent between the R server and the browser client —
input changes arriving, output/UI updates going out. Set it to `"send"` or
`"recv"` for one direction only. Useful when the symptom is "the UI isn't
updating" or "my input isn't reaching the server."

```r
# Partial snippet: run before shinyApp(ui, server)
options(shiny.trace = TRUE)
```

## Visualize the reactive graph: reactlog

For "why did/didn't this recompute," {reactlog} records every reactive
read/write/invalidate as an interactive graph. Enable recording with
`options(shiny.reactlog = TRUE)` before launching, then call
`shiny::reactlogShow()` after interacting with the app — it defaults to the
log Shiny just recorded, where `reactlog::reactlog_show()` requires you to
pass one.

```r
# Partial snippet: run before shinyApp(ui, server); requires the reactlog package
options(shiny.reactlog = TRUE)
```

## Distinguish "not ready yet" from "actually broken": `req()` and `validate()`

Many apparent bugs are really an input that hasn't been supplied yet.
`req(x)` silently stops the current reactive/output when `x` is falsy,
missing, or empty. `validate(need(cond, message))` is similar but shows
`message` in the output, for conditions the user should see and fix.

```r
library(shiny)

ui <- fluidPage(
  numericInput("n", "Rows to preview", value = NA),
  tableOutput("preview")
)

server <- function(input, output, session) {
  data <- mtcars
  output$preview <- renderTable({
    req(input$n)                                          # wait quietly for a value
    validate(need(input$n > 0, "Rows must be positive"))   # show this message otherwise
    head(data, input$n)
  })
}

shinyApp(ui, server)
```

If a render function errors every time the app starts, check whether it's
really an unset input before chasing a logic bug — add `req()` first.

## `print()` outside a reactive context errors

A bare `print(input$x)` at the top level of `server`, outside
`reactive()`/`observe()`/`render*()`/`isolate()`, throws "Operation not
allowed without an active reactive context." For an unreactive one-off
read, wrap it in `isolate()`.

```r
# Partial snippet: inside a server function
observe({
  print(input$x)   # OK: observe() provides a reactive context
})

# print(input$x)    # errors here: server's top level has no reactive context
```

## Quick reference

| Function/option | Purpose |
|---|---|
| `browser()` | Pause and inspect at a specific line |
| `options(shiny.error = browser)` | Drop into `browser()` automatically on error |
| `options(shiny.fullstacktrace = TRUE)` | Show full (not shortened) stack traces |
| `options(shiny.trace = TRUE)` | Print websocket messages between server and client |
| `options(shiny.reactlog = TRUE)` + `shiny::reactlogShow()` | Record and visualize the reactive graph |
| `req(x)` | Silently stop if `x` is missing/falsy |
| `validate(need(cond, msg))` | Show `msg` in the output if `cond` is false |

## Common mistakes

- Adding and removing `print()` calls while rerunning the app → set
  `options(shiny.error = browser)` once, or place a single `browser()` at
  the suspected line.
- An output errors only when the app first loads → the input it depends on
  probably isn't set yet; guard with `req()` before assuming a logic bug.
- Reading `input$x` directly in `server`'s top level, not inside
  `reactive()`/`observe()`/`render*()` → errors with "no active reactive
  context"; wrap it in `observe()` or `isolate()`.
- Confused about why a reactive did or didn't recompute → don't guess;
  record it with `options(shiny.reactlog = TRUE)` and inspect the graph.
- Forgetting to turn `shiny.trace`/`shiny.fullstacktrace` back off → both
  are noisy; reset each to `FALSE` once done.
