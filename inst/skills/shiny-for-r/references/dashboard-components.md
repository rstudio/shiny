# Dashboard Components in Shiny for R

## Overview

Build dashboard content with {bslib}'s component functions — `card()`,
`value_box()`, `accordion()`, `tooltip()`/`popover()` — instead of hand-rolled
`div(class = "card")` HTML, which misses the theme-aware CSS, full-screen
expansion, and fill behavior these functions give you for free. This
reference covers the bslib components that fill a dashboard's body.

## The card: the primary content unit

`card()` groups related content behind a border. `card_header()` and
`card_footer()` must be direct children; other children get wrapped in an
implicit `card_body()`. Add `full_screen = TRUE` on cards with a plot or table.

```r
# Partial snippet: inside a page_* function
card(
  full_screen = TRUE,
  card_header("Bill length by species"),
  plotOutput("scatter"),
  card_footer("Source: penguins dataset")
)
```

Use `min_height`/`max_height` to bound size in a filling layout, and
multiple `card_body()` sections (`fill = FALSE` where they shouldn't
stretch) to combine fixed and resizable regions.

## Local controls in the card header: toolbars

Page-wide filters belong in a sidebar; controls that change one card's
display (period, grouping, sort order) belong in its `card_header()`, via
`toolbar()`. Requires bslib >= 0.11.0.

```r
# Partial snippet: inside a card()
card_header(
  "Revenue trend",
  toolbar(
    toolbar_input_select("period", "Period", choices = c("Week", "Month", "Year")),
    toolbar_divider(),
    toolbar_input_button("download", "Download")
  )
)
```

`toolbar_input_button()` hides its label as a tooltip when an `icon` is
supplied. Update controls from the
server with `update_toolbar_input_button()`/`update_toolbar_input_select()`.

## KPI tiles: value_box

A `value_box()` pairs a `title` with a prominent `value`, plus an optional
`showcase` icon and context text. `bsicons::bs_icon()` (from the small
{bsicons} package) supplies Bootstrap icons for `showcase` and elsewhere.

```r
# Partial snippet: inside a page_* function
value_box(
  title = "Net revenue",
  value = "$1.2M",
  "Up 30% vs last month",
  showcase = bsicons::bs_icon("cash"),
  theme = "primary"
)
```

Group value boxes with `layout_column_wrap(width = 1/3, fill = FALSE, ...)`
so they keep a natural height. `showcase_layout`
(`showcase_left_center()`, `showcase_top_right()`, `showcase_bottom()`)
controls showcase placement.

## Secondary explanation: tooltip and popover

Use `tooltip()` for a short, hover-triggered message, and `popover()` when
content needs several lines or interactive inputs. Neither should hold
essential information — both are easy to miss on touch devices.

```r
# Partial snippet: inside a card_header()
tooltip(
  bsicons::bs_icon("info-circle"),
  "Completed orders divided by initiated checkouts."
)
```

When `trigger` renders as *several* top-level elements, only the last one is
hoverable — `tooltip(tagList("Label", bsicons::bs_icon("info")), ...)` puts
the tooltip on the icon alone. Wrap them in a single `span()` or `div()`
when the whole thing should be the trigger.
`popover()` follows the same pattern but opens on click and can hold
inputs. Give either an `id` to drive it via `toggle_tooltip()`/
`toggle_popover()` or `update_tooltip()`/`update_popover()`.

## Collapsible sections: accordion

`accordion()` holds `accordion_panel()` children for progressive
disclosure — useful for grouping many sidebar inputs.

```r
# Partial snippet: inside sidebar(), inside page_sidebar()
accordion(
  open = "Filters",
  accordion_panel("Filters", selectInput("species", "Species", c("A", "B"))),
  accordion_panel("Advanced", sliderInput("alpha", "Transparency", 0, 1, 0.8))
)
```

An `accordion()` that is an immediate child of `sidebar()` renders flush.
Control panels from the server with `accordion_panel_open()`,
`accordion_panel_close()`, `accordion_panel_set()` (needs an `id`).

## Specialized inputs

- `input_switch(id, label)` — modern on/off toggle, alternative to `checkboxInput()`.
- `input_dark_mode(id = "mode")` — toggles Bootstrap 5.3 light/dark mode.
- `input_task_button(id, label)` — action button with a built-in busy state;
  pair with a long-running task object and `bind_task_button()` for slow work.

## A full dashboard: value boxes plus a full-screen card

```r
library(shiny)
library(bslib)

ui <- page_fillable(
  layout_column_wrap(
    width = 1 / 2,
    fill = FALSE,
    value_box(title = "Users", value = "1,234", theme = "primary"),
    value_box(title = "Revenue", value = "$56K", theme = "success")
  ),
  card(full_screen = TRUE, card_header("Detailed trend"), plotOutput("trend"))
)

server <- function(input, output, session) {
  output$trend <- renderPlot(plot(1:20, type = "l"))
}

shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `card()` / `card_header()` / `card_body()` / `card_footer()` | Content container and sections |
| `full_screen = TRUE` on `card()` | Expand-to-full-window button |
| `value_box(title, value, ...)` | KPI or headline metric tile |
| `toolbar()` / `toolbar_input_button()` / `toolbar_input_select()` | Compact card header/footer controls |
| `tooltip()` | Short, hover-triggered message |
| `popover()` | Click-triggered richer content |
| `accordion()` / `accordion_panel()` | Collapsible, organized sections |
| `input_switch()` | On/off toggle |
| `input_dark_mode()` | Light/dark color mode toggle |
| `input_task_button()` | Action button with built-in loading state |

## Common mistakes

- **Hand-rolling `div(class = "card")` HTML instead of `card()`.** Loses
  theming, full-screen expansion, and fill behavior.
- **Wrapping toolbar buttons/selects directly in `card_header()`.** Use
  `toolbar()` for compact alignment and spacing.
- **Value boxes stretching to fill a page.** Wrap them in
  `layout_column_wrap(..., fill = FALSE)`.
- **Using a popover for essential information.** Hover/click content is
  easy to miss; keep required content in the main flow.
- **Icon-only toolbar buttons with no label.** Always pass a meaningful
  `label`; bslib shows it as a tooltip, keeping it accessible.
- **Accordion not flush inside a sidebar.** It must be an immediate child
  of `sidebar()`, not wrapped in another element.
