# Designing polished dashboards in Shiny for R

## Overview

Treat a dashboard as a decision surface, not a gallery of widgets. This
reference prevents the "20-widget wall" anti-pattern — every column a
filter, every metric a card, no visual hierarchy — by grounding layout
choices in the decision the dashboard serves. It assumes you know the
component APIs (`page_sidebar()`, `layout_column_wrap()`, `card()`,
`value_box()`, `sidebar()`, `accordion()`, `tooltip()`, `popover()`,
`full_screen`); this is about *when* and *why* to reach for them.

## Lead with the decision, not the data

Before writing UI code, identify the audience's decision, three to five
headline metrics with a period or comparison ("Revenue, last 30 days vs.
prior period" — not just "Revenue"), the few global filters affecting most
of the page, the chart(s) explaining movement in those metrics, and the
table for drilling into records. A conventional reading order is: title and
status line, a row of KPIs, one or two primary charts, a detailed table.
Use `page_sidebar()` for one shared workflow; reach for a navbar only when
sections support genuinely different tasks.

## Information hierarchy: KPIs top, detail below

Put headline numbers in `value_box()` tiles across the top, grouped with
`layout_column_wrap(fill = FALSE)` so they keep a natural height. Reserve
`card()` for whatever explains those numbers. Feed every KPI, chart, and
table from one filtering `reactive()`:

```r
# Partial snippet: inside a server function
filtered_sales <- reactive({
  data <- sales
  if (length(input$region) > 0) {
    data <- data[data$region %in% input$region, ]
  }
  data
})
```

If two cards can disagree about "the current selection", the dashboard is
really several disconnected views sharing a page.

## Progressive disclosure over cramming

Don't put everything on screen just because it fits. Use `full_screen =
TRUE` on a chart or table worth inspecting closely, rather than enlarging
the card by default; `tooltip()` for a one-line definition instead of a
permanent caption; `popover()` for a secondary control most visits won't
need; and `accordion()` to collapse advanced filters. None should hold
information required to interpret the primary view.

## Sensible filter defaults, and grouping controls

A dashboard's initial state must already show something meaningful — don't
ship one that opens with an empty `selectInput()` and a blank chart.
Default date ranges to a recent period, categorical filters to "all" or the
most common value, and multi-selects to every currently valid choice.
Design deliberately for combinations that yield zero rows — check
`nrow(filtered_sales()) == 0` and render an intentional message in the
card, rather than letting a spinner hang or a plot error on an empty data
frame.

A long, flat column of sidebar inputs is a sign to reorganize. Keep global
filters in `sidebar()`, and move less-frequently used ones into
`accordion()` panels inside it so the default view stays short. A display
option that affects only one card — sort order, grouping — belongs in that
card's header, not the page-level sidebar.

## One visual system, then verify it

Pick a single bslib theme (or `_brand.yml` source) before styling
individual cards, and reuse one accent color plus neutrals throughout.
Use the same color for a category everywhere. Format
values before they reach the UI — `27.5%`, not `0.274991` — and keep marks
readable in light and dark mode.

Before calling a dashboard done, open it in a browser and check: resizing
to mobile width; dark mode via `input_dark_mode()` if present; a long
label or large number (nothing overflows a value box or table column); an
empty-result filter combination; every `full_screen` card expanded and
closed again; and the console/server log for stray warnings.

## Quick reference

| Guidance | Component |
|---|---|
| Headline metrics | `value_box()` in `layout_column_wrap(fill = FALSE)` |
| Supporting detail | `card()` |
| Inspect a chart/table closely | `full_screen = TRUE` |
| One-line definition | `tooltip()` |
| Secondary control or explanation | `popover()` |
| Rarely used filters | `accordion()` inside `sidebar()` |
| Shared filtering logic | one `reactive()` read by every output |
| Consistent light/dark styling | one bslib theme or `_brand.yml` |

## Common mistakes

- **Every column becomes a filter, every metric a card.** Pick the metrics
  and filters that matter; cut the rest.
- **Sidebar with twenty flat inputs.** Group less-common filters in
  `accordion()` panels.
- **Each output recomputes its own filtered data.** Centralize filtering in
  one `reactive()`; duplicated expressions drift and disagree.
- **Dashboard opens blank.** Give filters defaults that produce a
  meaningful result on first load.
- **Essential information hidden in a tooltip or popover.** Both are easy
  to miss on touch; keep required content in the main flow.
- **Charts and maps recolor the same categories differently.** Reuse one
  categorical color mapping everywhere.
- **Shipped without resizing the browser or trying dark mode.** A short
  visual pass catches most layout regressions before users do.
