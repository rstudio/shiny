# The Shiny for R ecosystem

## Overview

Shiny's core package covers reactivity, inputs, and outputs, but most
polished apps also lean on a handful of companion packages for grids,
charts, maps, chat UIs, testing, and diagnostics. This reference prevents
two anti-patterns: reinventing what a companion package already does well
(hand-rolled sortable/filterable HTML tables, a bespoke reactive-graph
logger), and reaching for the wrong tool because the right one wasn't on
the radar. These packages ship, or should ship, their own agent skills
that go deeper into their APIs; this table only routes you to the right
package and entry point, not how to use it.

## Routing table

| Package | Reach for it when | Entry point |
|---|---|---|
| {DT} | You need an interactive data grid with sorting, filtering, and paging in the browser. | `DT::renderDT()` (pair with `DT::DTOutput()` in the UI) |
| {reactable} | You want a modern, styled, R-Markdown-friendly table with custom cell rendering. | `reactable::renderReactable()` (pair with `reactable::reactableOutput()`) |
| {plotly} | You need an interactive chart (zoom, hover tooltips, pan) rather than a static plot. | `plotly::renderPlotly()`, or wrap an existing ggplot2 object with `plotly::ggplotly()` |
| {leaflet} | You're displaying geographic/map data with pan, zoom, and markers. | `leaflet::renderLeaflet()` (pair with `leaflet::leafletOutput()`) |
| {shinychat} + {ellmer} | You're building a chat UI backed by an LLM. {ellmer} handles the model call; {shinychat} renders the conversation. | `shinychat::chat_ui()` in the UI, `ellmer::chat_anthropic()` (or another `chat_*()` provider) for the model |
| {shinytest2} | You need automated, browser-driven end-to-end tests instead of (or in addition to) `testServer()`. | `shinytest2::record_test()` to generate a test interactively |
| {reactlog} | You need to see the reactive graph itself — what invalidated what, and in what order — to debug a reactivity puzzle. | `reactlog::reactlog_enable()` before running the app, then `shiny::reactlogShow()` |
| {htmltools} | You're building custom UI components and need to construct or combine HTML tags, or bundle CSS/JS as a dependency. | `htmltools::tags` (or `tagList()`) for markup; `htmltools::htmlDependency()` for assets |
| {mirai} | You need to run slow or CPU-bound work off the main R process, without blocking other sessions (or, with `ExtendedTask`, the current one). | `mirai::mirai()`, with `mirai::daemons()` to set up reusable background processes |
| {promises} | You're composing async steps — chaining, error handling — around whatever produced the async value. | `promises::then()` piped with `\|>`, plus `promises::catch()`; use `promises::future_promise()` to run {future} work as a promise |
| {thematic} | You want your R plots (base, ggplot2, lattice) to automatically match the app's bslib theme, including dark mode. | `thematic::thematic_shiny()`, called once before the app runs |

## Common mistakes

- **Hand-rolling an HTML table for sorting/filtering.** Reach for {DT} or
  {reactable} instead of rebuilding paging and column sorting from scratch.
- **Building a static plot when users expect to hover or zoom.** Use
  `plotly::renderPlotly()`, or wrap an existing ggplot2 object with
  `plotly::ggplotly()`, rather than adding your own JS for interactivity.
- **Writing a custom chat widget for an LLM feature.** {shinychat} already
  renders streaming assistant turns; pair it with {ellmer} for the model
  call instead of assembling one from raw HTML and JS.
- **Only ever testing with `testServer()`.** It cannot see rendered HTML,
  CSS, or JS widgets — reach for {shinytest2} when the browser-rendered
  result itself is what needs checking.
- **Debugging a reactivity puzzle by adding `print()` calls everywhere.**
  {reactlog} shows the actual invalidation graph, usually faster than
  guessing from scattered log lines.
- **Calling `future::future()` directly for background work.** It blocks
  the main R process whenever every {future} worker is busy. Wrap it in
  `promises::future_promise()`, which hands back a promise right away and
  starts the work once a worker frees up.
- **Restyling every plot function by hand to match dark mode.** Call
  `thematic::thematic_shiny()` once and let base R, ggplot2, and lattice
  plots pick up the app's theme automatically.
- **Constructing HTML with `paste0()` and `HTML()`.** {htmltools} tag
  functions compose safely and let `htmlDependency()` bundle any CSS/JS the
  component needs.
