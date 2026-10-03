# Resuming Shiny for R sessions

## Overview

`enableResume()` keeps a user's state when the connection drops or the page
reloads. Shiny saves the session as it goes; a session that reconnects or
reloads starts again from that state instead of from zero. Use it instead of
hand-built "save my work" logic when the loss is an interruption (a network
blip, a server restart, a reload), not a deliberate reset.

What comes back: labelled `reactiveVal()` and `reactiveValues()` values,
`input`, uploaded files and `session$userData`. What is recomputed:
`reactive()` expressions and outputs. What does **not** run again: an
`observeEvent()` whose event already happened (it would repeat a side
effect such as sending an email). What is never saved: observers, outputs,
closure state (`<<-`) and async work in flight.

## Turning it on

Call `enableResume()` at the top level of `app.R` (before `shinyApp()`) or in
`global.R` — not in `server.R` and not inside the server function.

```r
library(shiny)
enableResume(reload = "ask")

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
argument turns it off for the app. `reload` says what a user's page reload
does: `"ask"` (default) offers **Pick up where you left off** or **Start
fresh**, `"resume"` resumes with a "Start fresh instead" toast, `"fresh"`
starts over.

## The rules of reactives

State comes back all or nothing: if any source that should restore cannot,
the session resumes from its inputs only. Four habits keep that from
happening.

1. **Label sources created in loops or in packaged code.** An installed
   package keeps no source references, so an unlabelled source there has no
   identity and blocks resume until it is labelled or marked.
2. **Hold reference objects in a `reactive()`, not a `reactiveVal()`.** A
   connection, pool or R6 client cannot be saved; in a `reactive()` it is
   rebuilt on first read.
3. **Derive page content with `renderUI()`.** Content an `observeEvent()` put
   on the page imperatively (`insertUI()`, new `update*Input()` choices,
   `showModal()`) does not come back on a reloaded page, because that
   handler does not run again.
4. **Accumulate into a reactive value from `observeEvent()`, not a plain
   `observe()`.** A plain observer has no event to hold back, so it runs on
   resume and applies its change a second time.

```r
# Rule 1: label sources created in loops or in packaged code.
for (i in 1:3) local({ v <- reactiveVal(0, label = paste0("v", i)) })
# Rule 2: hold reference objects in a reactive(), not a reactiveVal().
conn <- reactive(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
# Rule 3: derive page content with renderUI().
output$panel <- renderUI(if (input$show) wellPanel("Details"))
# Rule 4: accumulate from observeEvent(), not a plain observe().
observeEvent(input$plus, count(count() + 1))
```

To leave one value or one observer out, call `disableResume(x)` on it. A
marked observer runs on resume exactly as on a fresh start: use it for a
handler whose job is to rebuild something outside the reactive graph.

## Reading the report

`session$resumeReport()` returns one row per value and input with whether it
restores and, if not, why. Printing groups the rows by reason. Use it in a
test:

```r
testServer(server, {
  session$setInputs(plus = 1)
  stopifnot(all(session$resumeReport()$adoptable))
})
```

## After a page reload

`session$reload()` is a reset: it discards the saved state and the page
starts fresh. A reload Shiny makes itself (the UI changed across a restart,
or autoreload in development) resumes without asking. A fresh page gets the
saved input values pushed into its widgets, so a `renderUI()` widget shows
what the user had.

## After an error

An unhandled error in an observer still ends the session. With resume on,
saving stops at the error and the page offers **Resume**, which returns to
the state saved just before it, or **Start over**. The user's last change
may already have taken effect before the error, so Resume lands slightly
before it, not exactly on it. The dialog shows the error message unless
`options(shiny.sanitize.errors = TRUE)`.

## Known limitations

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
| `enableResume(reload, appVersion, maxSize, maxNodeSize, debounce)` | Turn resume on for the app and configure it |
| `disableResume()` | Turn resume off for the app |
| `disableResume(x)` | Leave a `reactiveVal()`, `reactiveValues()` or observer out |
| `reactiveValues(..., .label)` | Name an object across sessions |
| `session$resumeReport()` | Why each value would or would not restore |
| `session$reload()` | Reload the page and start fresh |
