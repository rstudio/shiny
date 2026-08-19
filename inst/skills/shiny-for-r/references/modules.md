# Modules in Shiny for R

## Overview

A module is a reusable pair of functions — a UI function and a server
function — that can be dropped into an app, or another module, more than
once without id collisions. `NS()` and `moduleServer()` namespace every
input/output id automatically. Reach for a module instead of hand-prefixing
ids like `"counter1_button"` / `"counter2_button"`: manual prefixing is
brittle, easy to get out of sync, and defeats the reuse a module provides.

A module has two halves linked by a shared instance `id`:

- The UI function creates a namespace function with `ns <- NS(id)` and wraps
  every input/output id it creates in `ns(...)`.
- The server function is wrapped in `moduleServer(id, function(input, output,
  session) { ... })`. Inside that inner function, refer to ids by their bare
  name (`input$button`) — `moduleServer()` already scoped `input`, `output`,
  and `session` to the namespace.

Calling the UI function and `moduleServer()` with the **same id** wires that
UI instance to that server instance.

## End-to-end example: a counter module used twice

```r
library(shiny)

# --- Module UI: build ns <- NS(id) and wrap every id in ns(). ---
counterUI <- function(id, label = "Increment") {
  ns <- NS(id)
  tagList(
    h4(label),
    actionButton(ns("button"), label),   # bare id, wrapped in ns()
    verbatimTextOutput(ns("out"))
  )
}

# --- Module server: moduleServer() scopes input/output/session. ---
counterServer <- function(id, start = 0) {
  moduleServer(id, function(input, output, session) {
    count <- reactiveVal(start)

    observeEvent(input$button, {       # bare id, no namespace needed here
      count(count() + 1)
    })

    output$out <- renderPrint(count())

    count                              # expose a reactive value to the caller
  })
}

# --- App uses the module twice; each instance needs a unique id. ---
ui <- fluidPage(
  counterUI("counter1", "Counter 1"),
  counterUI("counter2", "Counter 2"),
  verbatimTextOutput("total")
)

server <- function(input, output, session) {
  counterServer("counter1")
  total <- counterServer("counter2", start = 100)

  output$total <- renderPrint({
    paste("Second counter:", total())
  })
}

shinyApp(ui, server)
```

## Pass data in, get values out

- **In:** add parameters after `id` on both the UI and server functions. To
  pass a reactive value, hand the caller's reactive itself — for example
  `counterServer("id", data = my_reactive)` — and call `data()` inside the
  module. Passing `input$x` (a plain value read once) instead of `reactive({
  input$x })` breaks reactivity: the module never sees later changes.
- **Out:** `return` a `reactiveVal()`, `reactive()`, or plain value from the
  server function, as `count` and `total` do above. The caller reads it like
  any other reactive.

## Namespacing inside the module: `session$ns()`

Everything created through the arguments Shiny hands you is namespaced
automatically. The one place you must namespace by hand is UI built
dynamically inside the module, such as with `renderUI()` or `insertUI()`:
use `session$ns(id)`, the server-side equivalent of `ns <- NS(id)`.

```r
# Partial snippet: inside moduleServer(id, function(input, output, session) { ... })
output$dynamic <- renderUI({
  textInput(session$ns("extra"), "Extra field")   # must use session$ns() here
})
```

## Nesting modules

A module's UI function can call another module's UI function, and a
module's server function can call another module's `moduleServer()`. Because
each module only ever uses bare ids internally, Shiny composes the full
namespace path (`outer-inner-button`) for you — never build that string
yourself.

```r
# Partial snippet: a module server that contains a nested module
outerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    inner_count <- counterServer("inner")   # nested module instance
    observeEvent(input$reset, {
      # ids here are still bare relative to "outer"
    })
    inner_count
  })
}
```

## Quick reference

| Function | Purpose |
|---|---|
| `NS(namespace, id = NULL)` | Build a namespacing function for a module's UI (`ns <- NS(id)`) |
| `moduleServer(id, module)` | Run a server function in a namespaced scope matching a UI instance |
| `session$ns(id)` | Server-side namespacing, for ids built dynamically inside the module |

## Common mistakes

- Manually prefixing ids inside a module (`"counter1_button"`) instead of
  wrapping with `ns()`/using the bare id inside `moduleServer()` → breaks the
  automatic wiring; always use the bare id and let `NS()`/`moduleServer()`
  namespace it.
- Forgetting `session$ns()` in `renderUI()` (or other dynamically generated
  UI) inside a module → the new input renders with an unnamespaced id and
  never reaches `input$...` inside the module; wrap it in `session$ns(...)`.
- Passing `input$x` into a module instead of `reactive({ input$x })` → the
  module receives a frozen value instead of a live reactive and never sees
  later updates; always pass reactives unevaluated, and call them inside the
  module.
- Using two module instances with the same id (`counterUI("a")` twice) →
  duplicate DOM ids and one server scope silently overwriting the other; give
  each instance a distinct id.
- Reaching into a module's inputs from the parent with a guessed string like
  `input[["counter1-button"]]` → fragile and breaks if the module is
  renamed or nested; return the value from the module's server function
  instead.
