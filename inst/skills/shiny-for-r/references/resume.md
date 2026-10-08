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
sets `SHINY_RESUME=TRUE`; the app's own call wins. `disableResume()` with no
argument turns it off.

## The rules of reactives

State comes back all or nothing: if a labelled value cannot be saved, the
session resumes from its inputs only. Four habits keep resumed sessions
complete and correct.

1. **Label sources created in loops or through helpers.** A source made in
   `lapply()` or by a wrapper function has no name to infer, so it is not
   saved.
2. **Hold reference objects in a `reactive()`, not a `reactiveVal()`.** A
   connection, pool or R6 client cannot be saved; in a `reactive()` it is
   rebuilt on first read.
3. **Build structure from state, not in handlers.** A handler that defines
   an output, starts a module server or creates an observer does not run
   again after a resume. Keep the ids in a `reactiveVal()`, render with
   `renderUI()`, and start module servers from a plain `observe()`.
4. **Accumulate into a reactive value from `observeEvent()`, not a plain
   `observe()`.** A plain observer has no event to hold back, so it runs on
   resume and applies its change again.

```r
# Rule 1: label sources created in loops.
counts <- lapply(1:3, function(i) reactiveVal(0, label = paste0("count", i)))
# Rule 2: hold reference objects in a reactive().
conn <- reactive(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
# Rule 3: build structure from state.
ids <- reactiveVal(character(0))
observeEvent(input$add, ids(c(ids(), paste0("panel", length(ids()) + 1))))
output$panels <- renderUI(lapply(ids(), panelUI))
observe(for (id in ids()) panelServer(id))
# Rule 4: accumulate from observeEvent().
observeEvent(input$plus, count(count() + 1))
```

(`panelServer()` must tolerate being started again for an id it already
serves, for example by tracking started ids in a plain environment.)

To leave one value, observer or `ExtendedTask` out, call `disableResume(x)`
on it. A marked observer runs on resume exactly as on a fresh start.

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

## Quick reference

| Function | Purpose |
|---|---|
| `enableResume(appVersion, maxSize, maxNodeSize, debounce)` | Turn resume on for the app and configure it |
| `disableResume()` | Turn resume off for the app |
| `disableResume(x)` | Leave a `reactiveVal()`, `reactiveValues()`, observer or `ExtendedTask` out |
| `reactiveValues(..., .label)` | Name an object across sessions |
| `session$resumeReport()` | Why each value would or would not restore |
