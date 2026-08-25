---
name: shiny-for-r
description: "Building, styling, testing, debugging, or observing a Shiny for R app - library(shiny), shinyApp(), runApp(). Index skill: read this, then open the linked reference for the task. Covers reactivity (reactive/observe/req/isolate/bindEvent); modules; async promises and ExtendedTask; bslib layouts, navigation, cards, value boxes, and theming (incl. thematic and brand.yml); dynamic UI; plots with click/brush interaction; tables, file upload/download; notifications, modals, progress, validation; bookmarking; custom JS components; testServer testing; debugging; OpenTelemetry; and when to reach for DT, plotly, leaflet, shinychat, or shinytest2. Use when writing or changing any Shiny for R app, especially a dashboard, or when tempted to hand-roll what the framework provides - custom HTML tables, fake tabs, polling loops, blocking reactive work, or print-debugging server state."
license: MIT
metadata:
  author: Posit Software, PBC
  version: "1.0"
---

# Shiny for R

A Shiny app has a `ui` object (or function), a `server(input, output,
session)` function, and `shinyApp(ui, server)` to run them together. The
reactive graph is the engine: reading a reactive source (`input$x`, a
`reactive()`, a `reactiveVal()`) inside a reactive context registers a
dependency, so changing that source re-runs everything that read it — you
never call outputs or schedule updates yourself.

This skill is an **index**. Find your task below and **read the linked
reference file before writing code** for that area. Read the one or two
references your task actually touches, not all of them.

Not every corner of Shiny has a reference here — plain inputs
(`selectInput()`, `actionButton()`, ...), deployment, and app performance
tuning are not covered. For those, work from your own knowledge of the
package and its help pages.

## Foundations

| Topic | Use when | Reference |
|---|---|---|
| Reactivity | A value should recompute or an output update as inputs change; choosing between `reactive()`/`observe()`/`observeEvent()`; `req()`, `isolate()`, `bindEvent()`, timers, polling | `references/reactivity.md` |
| Modules | A reusable, repeatable UI+server component; avoiding input/output id collisions across copies | `references/modules.md` |
| Session lifecycle | Per-session cleanup (`onSessionEnded()`), flush hooks, unhandled errors, per-session vs app-level scope, `session$userData` | `references/session-lifecycle.md` |

## Async

| Topic | Use when | Reference |
|---|---|---|
| Async | Returning a `promises::promise` from a render/observer so R keeps serving other sessions during slow I/O | `references/async.md` |
| Extended tasks | Running slow work in a background process while the session that started it stays fully interactive; task buttons | `references/extended-tasks.md` |

## Dashboard building

| Topic | Use when | Reference |
|---|---|---|
| Dashboard design | Turning a dataset or brief into a polished analytical dashboard; information hierarchy, shared filters, responsive layout, empty states, final visual/functional pass | `references/dashboard-design.md` |
| Dashboard components | Composing bslib `card()`, `value_box()`, `accordion()`, `tooltip()`/`popover()` instead of hand-rolled `div()` markup | `references/dashboard-components.md` |
| Layouts | Arranging a page with bslib `page_sidebar()`, `layout_columns()`, `card()` instead of nested `fluidRow()`/`column()` pyramids | `references/layouts.md` |

## Navigation & dynamic UI

| Topic | Use when | Reference |
|---|---|---|
| Navigation | Grouping switchable content into a real tab container/navbar instead of faking tabs with buttons plus `conditionalPanel()` | `references/navigation.md` |
| Dynamic UI | UI that must change after render — `update*Input()`, `conditionalPanel()`, `renderUI()`/`uiOutput()`, or `insertUI()`/`removeUI()` | `references/dynamic-ui.md` |

## Outputs

| Topic | Use when | Reference |
|---|---|---|
| Plots | Pairing `renderPlot()`/`plotOutput()` for base/lattice/ggplot2 graphics; click/hover/brush interaction via `nearPoints()`/`brushedPoints()` | `references/plots.md` |
| Tables | Choosing `renderTable()` (static), the deprecated `renderDataTable()`, or `DT`/`reactable` for sortable/searchable interactive tables | `references/tables.md` |
| Files | `fileInput()` uploads and `downloadHandler()`/`downloadButton()` generated-file downloads | `references/files.md` |

## Feedback & state

| Topic | Use when | Reference |
|---|---|---|
| Feedback | Toasts/notifications, modal dialogs, progress bars, and input validation sent from the server, typically inside `observeEvent()` | `references/feedback.md` |
| Bookmarking | Saving/restoring app state via a shareable URL instead of a hand-built query string or custom persistence layer | `references/bookmarking.md` |

## Theming, assets & extending

| Topic | Use when | Reference |
|---|---|---|
| Theming & assets | Setting `bslib::bs_theme()` Sass variables, `thematic` for plot theming, `brand.yml`, or adding custom CSS/JS/image assets | `references/theming-assets.md` |
| Custom components | Integrating a third-party JS widget or bespoke interaction when no built-in input/output covers it | `references/custom-components.md` |

## Testing & observability

| Topic | Use when | Reference |
|---|---|---|
| Testing | Verifying server-side reactive logic with `testServer()` without starting a real Shiny process or browser | `references/testing.md` |
| Debugging | Dropping into a real R debugger (`browser()`, error breakpoints) instead of `print()`-debugging a running reactive graph | `references/debugging.md` |
| OpenTelemetry | Tracing sessions, flush cycles, and reactive/observer/output executions instead of log-scraping or ad hoc timing | `references/opentelemetry.md` |
| Ecosystem | Deciding whether a companion package (DT, plotly, leaflet, shinychat, shinytest2, etc.) already solves the problem before hand-rolling it | `references/ecosystem.md` |
