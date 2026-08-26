# Layouts in Shiny for R

## Overview

Build page structure by composing {bslib} layout functions — `page_sidebar()`,
`layout_columns()`, `card()` — instead of nesting `fluidRow()`/`column()`
pyramids or hand-written `<div class="row">` markup. A three-card grid built
as `fluidRow(column(4, ...), column(4, ...), column(4, ...))` is fragile,
doesn't participate in bslib's fill system, and takes more code than the
single-call bslib equivalent. Start every app with one page function, then
nest `sidebar()`, `layout_columns()`, and `card()` inside it. This reference
covers bslib first, then the shiny functions it supersedes.

## Choose a page function

```r
library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "My Dashboard",
  sidebar = sidebar(selectInput("species", "Species", choices = c("A", "B", "C"))),
  card(card_header("Plot"), plotOutput("scatter")),
  card(card_header("Summary"), verbatimTextOutput("summary"))
)

server <- function(input, output, session) {
  output$scatter <- renderPlot(plot(1:10))
  output$summary <- renderPrint(input$species)
}

shinyApp(ui, server)
```

- `page_sidebar(..., sidebar = , title = )` — single-page dashboard with
  sidebar and title. `...` comes first, so an unnamed argument is page
  content; always name `sidebar =`.
- `page_navbar(...)` — multi-page app with a top navbar; pages are `nav_panel()`s.
- `page_fillable(...)` — fills the viewport height; foundation the other two build on.
- `page_fluid(...)` — full-width page that scrolls normally, no filling.

## Arrange cards in a grid

`layout_columns()` divides space on a 12-column grid via `col_widths`;
values wrap to a new row past 12.

```r
# Partial snippet: inside a page_* function
layout_columns(
  col_widths = c(4, 8),
  card(card_header("Filters"), "..."),
  card(card_header("Main plot"), plotOutput("p"))
)
```

`layout_column_wrap()` is simpler for equal-width items: `width` is a
fraction (`1/3` = three per row) or a CSS length (`"250px"` = as many as fit).

```r
# Partial snippet: inside a page_* function
layout_column_wrap(
  width = 1 / 3,
  value_box(title = "Users", value = "1,234"),
  value_box(title = "Revenue", value = "$56K"),
  value_box(title = "Growth", value = "+12%")
)
```

## Put a sidebar anywhere with `layout_sidebar()`

`page_sidebar()` places a page-level sidebar. `layout_sidebar()` puts one
inside a card or region instead — set `fillable = TRUE` to preserve fill
behavior, and give `sidebar()` an `id` to read its open state as `input$<id>`.

```r
# Partial snippet: inside a card()
layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(position = "right", sliderInput("bins", "Bins", 5, 50, 20)),
  plotOutput("hist")
)
```

## Filling vs scrolling

Fill only activates when a container has a defined height and every ancestor
down to the page is fillable. `page_fillable()` (and `page_sidebar()`/
`page_navbar()`, `fillable = TRUE` by default) set page height to the
viewport; `card()` and `layout_columns()` are fill carriers by default. Set
`fillable = FALSE` for a scrolling document with natural output heights.
Value boxes should usually not fill — pass `fill = FALSE` to the wrapping
`layout_column_wrap()`.

## Superseded shiny layout functions

Pre-bslib apps (or apps that must stay on Bootstrap 3) use `fluidPage()` with
`sidebarLayout()` and the `fluidRow()`/`column()` grid:

```r
library(shiny)

ui <- fluidPage(
  titlePanel("Legacy Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Species", choices = c("A", "B", "C"))
    ),
    mainPanel(
      wellPanel(
        fluidRow(
          column(6, plotOutput("scatter")),
          column(6, verbatimTextOutput("summary"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$scatter <- renderPlot(plot(1:10))
  output$summary <- renderPrint(input$species)
}

shinyApp(ui, server)
```

- `fluidPage(...)` — full-width scrolling page; no sidebar or filling. It
  uses legacy Bootstrap 3 styling unless you pass `theme = bs_theme(...)`.
- `sidebarLayout(sidebarPanel(...), mainPanel(...))` — two-region layout.
- `fluidRow(column(width, ...), ...)` — 12-column grid; widths sum to at most 12.
- `fillPage(...)` — viewport-filling page predating `page_fillable()`.
- `titlePanel(title)` — sets the browser title and a heading.
- `wellPanel(...)` — gray-background grouping box; predecessor to `card()`.

## Migrating to bslib

| Superseded | bslib replacement | Why |
|---|---|---|
| `fluidPage()` | `page_sidebar()` / `page_navbar()` | Sidebar, filling, and a Bootstrap 5 theme by default |
| `sidebarLayout(sidebarPanel(), mainPanel())` | `page_sidebar(sidebar = sidebar(...), ...)` | Collapsible sidebar |
| `fluidRow(column(...))` | `layout_columns()` / `layout_column_wrap()` | Works with filling |
| `wellPanel()` | `card()` | Full-screen, headers/footers |
| `fillPage()` | `page_fillable()` | Propagates through cards/grids |
| `titlePanel()` | `title` argument on a `page_*()` function | One less nested element |

## Quick reference

| Function | Purpose |
|---|---|
| `page_sidebar(..., sidebar = )` | Single-page dashboard with sidebar + title |
| `page_navbar(...)` | Multi-page app with a top navbar |
| `page_fillable(...)` | Viewport-filling page, no scrolling |
| `page_fluid(...)` | Full-width scrolling page |
| `layout_columns(col_widths = , ...)` | 12-column grid with explicit widths |
| `layout_column_wrap(width = , ...)` | Equal-width wrapping tiles |
| `layout_sidebar(sidebar = , ...)` | Sidebar inside a card or region |
| `sidebar(...)` | Sidebar content container |

## Common mistakes

- **Nesting `fluidRow()`/`column()` pyramids for equal-width cards.** Use
  `layout_column_wrap(width = 1/n, ...)` in one call instead.
- **Content won't fill the window.** A plain `page_fluid()`/`fluidPage()`
  does not fill; switch to `page_fillable()`/`page_sidebar()`.
- **`height` is ignored inside a fill container.** Set `height`/`row_heights`
  on the container, or use a non-filling page.
- **Value boxes stretch too tall in a fillable page.** Add `fill = FALSE`
  to the wrapping `layout_column_wrap()`.
- **More than one page function.** An app has exactly one top-level
  `page_*()` or `fluidPage()`; nest layout containers inside it.
