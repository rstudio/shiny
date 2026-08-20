# Dynamic UI in Shiny for R

## Overview

UI does not have to be fixed at startup. Reach for the cheapest tool first:
`update*Input()` to change a control in place, `conditionalPanel()` to
show/hide static UI with no server round-trip, `renderUI()`/`uiOutput()` to
compute new UI reactively, and `insertUI()`/`removeUI()` only for UI that
must persist and accumulate. The anti-pattern this reference prevents is
reaching for `renderUI()` when `update*Input()` would do — regenerating a
whole input rebuilds its DOM element, losing focus and client state, where
`updateSelectInput()` and friends mutate the existing widget instead.

## Update an existing input: update*Input()

Every built-in input has a matching `update*Input()` function
(`updateSelectInput()`, `updateTextInput()`, `updateSliderInput()`, ...).
Call it inside `observe()`/`observeEvent()` — at server top level it runs
once with no dependency and never fires again.

```r
library(shiny)

ui <- fluidPage(
  selectInput("country", "Country", c("US", "CA")),
  selectInput("city", "City", character(0))
)

server <- function(input, output, session) {
  observe({
    cities <- list(US = c("NYC", "LA"), CA = c("Toronto", "Montreal"))
    updateSelectInput(session, "city", choices = cities[[input$country]])
  })
}

shinyApp(ui, server)
```

## Show/hide UI client-side: conditionalPanel()

`conditionalPanel(condition, ...)` wraps UI that stays in the DOM but is
toggled by a JavaScript expression evaluated in the browser — instant, no
server round-trip. `condition` reads `input.<id>`/`output.<id>` as
JavaScript, not R.

```r
library(shiny)

ui <- fluidPage(
  checkboxInput("advanced", "Show advanced options", FALSE),
  conditionalPanel(
    condition = "input.advanced",
    sliderInput("threshold", "Threshold", min = 0, max = 1, value = 0.5)
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
```

Its inputs keep their values while hidden. Use it only when the condition
is expressible over input/output values in JavaScript; a server-side
decision is a `renderUI()` job instead.

## Compute UI reactively: renderUI()

Put a `uiOutput(id)` placeholder in the UI and pair it with
`output[[id]] <- renderUI({...})` on the server. It re-runs when a reactive
value it reads changes, replacing the placeholder with whatever UI it
returns.

```r
library(shiny)

ui <- fluidPage(
  selectInput("kind", "Control", c("slider", "text")),
  uiOutput("control")
)

server <- function(input, output, session) {
  output$control <- renderUI({
    if (input$kind == "slider") {
      sliderInput("n", "N", min = 1, max = 100, value = 50)
    } else {
      textInput("label", "Label")
    }
  })
}

shinyApp(ui, server)
```

Use this when the *set* of controls must change, not just a value — e.g.
swapping a slider for a text box. For a value or choices, `update*Input()`
is cheaper and preserves widget state.

## Avoid stale reads: freezeReactiveValue()

Changing one input's choices can leave a *second*, dependent input
transiently invalid — downstream reactives see the old value for one tick,
often flashing an error. `freezeReactiveValue(x, name)` marks `x[[name]]`
(usually `input$name`) as frozen: reading it while frozen gets the same
silent stop as `req(FALSE)`. It thaws once the reactive flush completes.

## Inject/remove UI: insertUI() / removeUI()

Reach for these only when UI must persist and accumulate — e.g. a button
that adds a row each click. `selector` is a jQuery/CSS selector for where
to insert; inserted UI stays until explicitly removed.

```r
library(shiny)

ui <- fluidPage(
  actionButton("add", "Add field"),
  tags$div(id = "fields")
)

server <- function(input, output, session) {
  observeEvent(input$add, {
    id <- paste0("txt", input$add)
    insertUI(
      selector = "#fields",
      where = "beforeEnd",
      ui = textInput(id, paste("Field", input$add))
    )
  })
}

shinyApp(ui, server)
```

Remove with `removeUI(selector = ...)`, targeting the wrapper `<div>` (e.g.
`"div:has(> #txt1)"`) — inputs render wrapped, so removing the input alone
leaves the label behind.

## Full app: dependent selects

```r
library(shiny)

ui <- fluidPage(
  selectInput("data", "Data set", c("mtcars", "iris")),
  checkboxGroupInput("cols", "Columns (pick 2)", character(0)),
  plotOutput("plot")
)

server <- function(input, output, session) {
  observe({
    data <- get(input$data)
    freezeReactiveValue(input, "cols")
    updateCheckboxGroupInput(session, "cols", choices = names(data))
  })

  output$plot <- renderPlot({
    data <- get(input$data)
    cols <- input$cols
    if (length(cols) == 2) {
      plot(data[[cols[1]]], data[[cols[2]]])
    }
  })
}

shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `update*Input(session, inputId, ...)` | Change an input's value/choices in place |
| `conditionalPanel(condition, ...)` | Show/hide UI client-side, no round-trip |
| `renderUI()` + `uiOutput(id)` | Compute new UI reactively |
| `insertUI(selector, where, ui)` | Inject persistent, accumulating UI |
| `removeUI(selector)` | Remove UI added with `insertUI()` |
| `freezeReactiveValue(x, name)` | Suppress stale reads of `x[[name]]` during an update |

## Common mistakes

- **Using `renderUI()` to change a value or choices.** Rebuilds the DOM
  element, losing focus and client state — use `update*Input()`.
- **Calling `update*Input()` at server top level.** Runs once, never
  reacts — wrap it in `observe()`/`observeEvent()`.
- **Regenerating choices without `freezeReactiveValue()`.** Dependents
  briefly see the stale value, often flashing an error.
- **Reaching for `insertUI()` to swap input variants.** It accumulates and
  must be removed by hand; use `renderUI()` instead.
- **`removeUI("#txt1")` instead of the wrapper.** Inputs render wrapped in
  a `<div>`; target the wrapper, e.g. `"div:has(> #txt1)"`.
- **Using `conditionalPanel()` for a server-side decision.** Its condition
  is JavaScript over `input`/`output` only — that's `renderUI()`'s job.
