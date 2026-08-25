# Tables in Shiny for R

## Overview

`renderTable()`/`tableOutput()` render a small, static data frame as a plain
HTML `<table>` — fine for a filtered subset or a summary a user reads
top-to-bottom, not for anything a user needs to sort, page, or search.
`renderDataTable()`/`dataTableOutput()` used to be shiny's answer for larger,
interactive tables; shiny's own docs now mark it deprecated in favor of the
{DT} package. The anti-pattern this reference prevents is pushing a large
table (tens of thousands of rows) through `renderTable()`, which renders every
row as HTML on every update with no pagination, sorting, or search — for
anything beyond a small, already-filtered data frame, reach for {DT} or
{reactable} instead.

## Render a small static table: renderTable() / tableOutput()

`renderTable(expr)` takes an expression returning a data frame or matrix and
formats it with {xtable}; pair it with `tableOutput(outputId)`.

```r
library(shiny)

ui <- fluidPage(
  selectInput("species", "Species", choices = levels(iris$Species)),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderTable({
    iris[iris$Species == input$species, ]
  })
}

shinyApp(ui, server)
```

Formatting arguments on `renderTable()`: `striped=`, `hover=`, `bordered=`
(Bootstrap table styles), `spacing=` (`"xs"`/`"s"`/`"m"`/`"l"`), `width=`,
`align=` (e.g. `"lrc"` per column), `rownames=`/`colnames=` (include them?),
`digits=` (decimal places), and `na=` (string for missing values, default
`"NA"`). There is no pagination, sorting, or search — the whole table renders
every time `expr` re-runs, which is why this pair is only appropriate for
tables a person can scan directly on the page.

## Larger or interactive tables: renderDataTable() status

Shiny still ships `renderDataTable()`/`dataTableOutput()`, but its own
documentation marks them **deprecated**: "This function is deprecated, use
`DT::renderDT()` instead. It provides a superset of functionality, better
performance, and better user experience." The help topic is also flagged
internal (it no longer appears in the main function index).

Since shiny 1.8.1 they are also thin shims: when DT >= 0.32.1 is installed
they emit a deprecation message and hand off to `DT::renderDT()`/
`DT::DTOutput()`. Shiny's bundled legacy DataTables implementation runs only
when DT is absent, or when you opt back in with
`options(shiny.legacy.datatable = TRUE)`. Either way, call {DT} directly in
new code rather than routing through the shim.

## Ecosystem pointers for interactive grids

- {DT} (`DT::renderDT()` / `DT::DTOutput()`) is the direct, actively
  maintained successor to `renderDataTable()` — sorting, searching,
  pagination, server-side processing for large data, and row/cell selection.
- {reactable} (`reactable::renderReactable()` / `reactable::reactableOutput()`)
  gives a modern, more heavily styled table with expandable rows and custom
  cell rendering.

Both packages read like `renderTable()`: wrap a data frame in the package's
`render*()` function and pair it with the matching `*Output()` placeholder.

## Full app: renderTable of a filtered data frame

```r
library(shiny)

ui <- fluidPage(
  sliderInput("mpg_min", "Minimum mpg", min = 10, max = 35, value = 20),
  tableOutput("filtered")
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    mtcars[mtcars$mpg >= input$mpg_min, c("mpg", "cyl", "wt")]
  })

  output$filtered <- renderTable({
    filtered_data()
  }, rownames = TRUE, digits = 1)
}

shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `renderTable()` / `tableOutput()` | Static HTML table for small data frames |
| `renderDataTable()` / `dataTableOutput()` | Deprecated; use {DT} instead |
| `DT::renderDT()` / `DT::DTOutput()` | Interactive grid: sort, search, page, server-side data |
| `reactable::renderReactable()` | Modern styled table with expandable rows |

## Common mistakes

- **Pushing 100k rows through `renderTable()`.** It renders every row as HTML
  with no pagination — filter/aggregate the data first, or switch to {DT}
  with server-side processing.
- **Reaching for `renderDataTable()` in new code.** It is deprecated in
  shiny's own docs; use `DT::renderDT()`.
- **Expecting sorting/search from `renderTable()`.** It is a plain static
  `<table>` — those interactions require {DT} or {reactable}.
- **Re-rendering the whole table on every keystroke of a filter input.**
  Gate it reactively: `debounce(reactive(input$filter), 500)` waits for
  typing to pause, `throttle()` caps the rate, and
  `eventReactive(input$apply, ...)` (or `bindEvent()`) recomputes only when
  an explicit button is clicked. Either way, filter inside one `reactive()`
  so downstream renders share the result instead of recomputing it
  themselves.
