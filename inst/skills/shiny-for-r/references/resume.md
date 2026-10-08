# Resuming Shiny for R sessions

## Overview

`enableResume()` keeps a user's state when the connection drops. Shiny saves
the session as it goes; a page that reconnects, even to a restarted R
process, continues from that state instead of from zero. A page reload
still starts fresh.

What comes back: `reactiveVal()` and `reactiveValues()` values with a label
(an assigned name counts), `input`, uploaded files and `session$userData`.
What is recomputed: `reactive()` expressions and outputs. What does **not**
run again: an `observeEvent()` whose event already happened (it would
repeat a side effect such as sending an email). What is never saved:
observers, outputs, closure state (`<<-`) and async work in flight.

## Turning it on

Call `enableResume()` at the top level of `app.R` (before `shinyApp()`) or
in `global.R`, not in `server.R` and not inside the server function.

```r
library(shiny)
enableResume()

ui <- fluidPage(actionButton("plus", "+1"), textOutput("count"))
server <- function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  output$count <- renderText(count())
}
shinyApp(ui, server)
```

Resume is off unless the app calls `enableResume()` or the hosting platform
sets `SHINY_RESUME=TRUE`; the app's own call wins, and
`enableResume(FALSE)` turns it off.

## Rebuilding on resume

`session$onResume()` runs a function once when a page reconnects, after the
server function and before any observer or output, with saved values
already restored. Use it to rebuild what event handlers built, since those
handlers are held back:

```r
ids <- reactiveVal(character())
observeEvent(input$add, {
  id <- paste0("p", input$add)
  ids(c(ids(), id))
  insertUI("#panels", ui = panelUI(id))
  panelServer(id)
})
session$onResume(function(info) {
  for (id in ids()) panelServer(id)
})
```

`info$restored` is `TRUE` when saved state came back and `FALSE` when the
session resumed from its inputs only. It also fires with resume off, when
`session$allowReconnect(TRUE)` reconnects a page.

## The rules of reactives

State comes back all or nothing: if a labelled value cannot be saved, or the
app's R files changed and no `appVersion` is set, the session resumes from its
inputs only. Saved state that does not fit once the server function has run (a
labelled source with no saved value) ends the session. Four habits keep resumed sessions
complete and correct.

1. **Label sources created in loops or through helpers.** A source made in
   `lapply()` or by a wrapper function has no name to infer, so it is not
   saved.
2. **Hold reference objects in a `reactive()`, not a `reactiveVal()`.** A
   connection, pool or R6 client cannot be saved; in a `reactive()` it is
   rebuilt on first read. If one must live in a source, leave it out with
   `excludeFromResume()` and rebuild it in `session$onResume()`.
3. **Build structure from state.** A handler that defines an output, starts
   a module server or creates an observer is held back on resume. Derive
   the structure from state (`renderUI()`, an `observe()` over ids), or
   keep the ids in a `reactiveVal()` and rebuild in `session$onResume()`.
   For one output built from inputs, prefer `eventReactive()`.
4. **Accumulate into a reactive value from `observeEvent()`, not a plain
   `observe()`.** A plain observer has no event to hold back, so it runs on
   resume and applies its change again.

```r
# Rule 1: label sources created in loops.
counts <- lapply(1:3, function(i) reactiveVal(0, label = paste0("count", i)))
# Rule 2: hold reference objects in a reactive().
conn <- reactive(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
# Rule 3: one output built from inputs.
n <- eventReactive(input$go, input$n)
output$plot <- renderPlot(plot(seq_len(n())))
# Rule 4: accumulate from observeEvent().
observeEvent(input$plus, count(count() + 1))
```

To leave one value or `ExtendedTask` out of the saved state, wrap it in
`excludeFromResume()`, around the constructor or before the app first reads
it; after a resume it holds its initial value. Leave credentials in
`session$userData` out with `enableResume(userDataExclude = "token")`.

## Reading the report

`session$resumeReport()` returns one row per value and input with whether
it restores and, if not, why. Use it in a test:

```r
testServer(server, {
  session$setInputs(plus = 1)
  stopifnot(all(session$resumeReport()$adoptable))
})
```

## Known limitations

- A page reload starts fresh, and an unhandled error in an observer ends
  the session and deletes its saved state.
- Process-scoped values (temp file paths, process ids) serialize fine and
  are wrong after a server restart.
- `observeEvent(once = TRUE)` may fire once more after a resume.
- `eventReactive()` recomputes with the current inputs.
- Whether a hosting platform's storage survives a process restart is up to
  the platform; without a persistent volume a restarted container resumes
  from inputs only.
- A connection that drops while a page is resuming, before the resumed
  session first saves, resumes from inputs only on the next attempt.

## Quick reference

| Function | Purpose |
|---|---|
| `enableResume(enabled, appVersion, maxSize, maxNodeSize, debounce, userDataExclude)` | Turn resume on (or off with `FALSE`) for the app and configure it |
| `excludeFromResume(x)` | Leave a `reactiveVal()`, `reactiveValues()` or `ExtendedTask` out |
| `session$onResume(fun)` | Run code when a page reconnects, to rebuild what handlers built |
| `reactiveValues(..., .label)` | Name an object across sessions |
| `session$resumeReport()` | Why each value would or would not restore |
