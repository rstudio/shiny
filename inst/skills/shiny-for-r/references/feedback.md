# Transient feedback in Shiny for R

## Overview

Feedback that appears briefly and then goes away — toasts, dialogs, progress
bars — is sent from the **server**, almost always from inside an
`observeEvent()`, as overlays that Shiny renders and dismisses for you.
Do NOT fake a popup with `conditionalPanel()` + `renderUI()`, do NOT
`stop()` in a render function for an error the user caused (that shows a
scary red "Error:" box instead of a friendly message), and do NOT run a long
loop with no visible sign of progress. Use the built-in overlays below.

## Toast notifications

`showNotification(ui, action = NULL, duration = 5, closeButton = TRUE, id =
NULL, type = c("default", "message", "warning", "error"))` stacks a
non-blocking message in a corner and auto-dismisses it after `duration`
seconds (`NULL` keeps it up until removed). It returns an id; reuse that id
to update the notification, or pass it to `removeNotification(id)` to
dismiss it early.

```r
library(shiny)

ui <- fluidPage(actionButton("save", "Save"))

server <- function(input, output, session) {
  observeEvent(input$save, {
    id <- showNotification("Saving...", duration = NULL)
    Sys.sleep(1) # pretend to do work
    removeNotification(id)
    showNotification("Saved!", type = "message", duration = 3)
  })
}

shinyApp(ui, server)
```

## Modal dialogs

`modalDialog(..., title = NULL, footer = modalButton("Dismiss"), easyClose =
FALSE)` builds the dialog UI; `showModal()` displays it and `removeModal()`
closes it from the server. `easyClose = TRUE` allows dismissal by clicking
outside or pressing Escape; otherwise a `footer` button must do it. For a
confirmation pattern, give the footer a cancel button and an action button
that triggers the real work:

```r
library(shiny)

ui <- fluidPage(
  actionButton("delete", "Delete record"),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {
  status <- reactiveVal("Nothing deleted yet.")

  observeEvent(input$delete, {
    showModal(modalDialog(
      title = "Are you sure?",
      "This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete, {
    removeModal()
    status("Record deleted.")
  })

  output$status <- renderText(status())
}

shinyApp(ui, server)
```

## Progress bars

For a known sequence of steps in one scope, wrap the work in `withProgress(expr,
min = 0, max = 1, message = , detail = )` and call `incProgress(amount = 0.1)`
as each step finishes; the bar closes automatically when `withProgress()` exits.

```r
library(shiny)

ui <- fluidPage(actionButton("go", "Run"), verbatimTextOutput("done"))

server <- function(input, output, session) {
  result <- eventReactive(input$go, {
    withProgress(message = "Calculating", detail = "This may take a while...", value = 0, {
      for (i in 1:15) {
        incProgress(1 / 15)
        Sys.sleep(0.1)
      }
    })
    "Done!"
  })

  output$done <- renderText(result())
}

shinyApp(ui, server)
```

When the work spans multiple callbacks, use the R6 `Progress` class instead:
`Progress$new(session, min =, max =)` creates the panel, `$set(value =,
message =, detail =)`/`$inc(amount =)` update it, and `$close()` removes it —
call `$close()` yourself (e.g. via `on.exit()`).

## User-facing input errors: `validate()` versus `req()`

Inside a `render*()` function, `validate(...)` checks a list of conditions
and, on the first failure, stops and shows the message in place of the
output — styled as ordinary text, not a red error. Build conditions with
`need(expr, message)`, which fails with `message` when `expr` isn't
"truthy". Use `validate(need(...))` whenever the *user* needs to know what to
fix.

`req(...)` also stops on the first non-truthy value, but silently: no
message, output just doesn't update. Use `req()` for values not ready yet
(an empty input at startup) rather than values that are wrong.

```r
library(shiny)

ui <- fluidPage(
  selectInput("dataset", "Dataset", choices = c("", "mtcars", "iris")),
  tableOutput("head")
)

server <- function(input, output, session) {
  output$head <- renderTable({
    validate(need(input$dataset != "", "Please choose a dataset."))
    head(get(input$dataset))
  })
}

shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `showNotification()` / `removeNotification()` | Non-blocking toast; reuse `id` to update, remove it early |
| `modalDialog()` + `showModal()` / `removeModal()` | Blocking dialog; footer buttons dismiss or trigger action |
| `withProgress()` + `incProgress()` | Progress bar for a single scope of known steps |
| `Progress$new()`, `$set()`, `$inc()`, `$close()` | Progress bar spanning multiple callbacks/async work |
| `validate(need(...))` | Stop a render with a user-visible explanation |
| `req(...)` | Stop a render silently when a value isn't ready yet |

## Common mistakes

- `stop("bad input")` inside a `render*()` for an expected user mistake ->
  shows a red "Error:" box; use `validate(need(...))` for a friendly message.
- Faking a popup with `conditionalPanel()` + `renderUI()` -> use
  `modalDialog()` / `showModal()` instead.
- Calling `showNotification()`/`showModal()`/`Progress$new()` from a plain
  R script or the console -> they error; they need an active session
  (`getDefaultReactiveDomain()`). Anywhere inside `server` is fine — top
  level, a render function, or an observer.
- Forgetting to capture the id from `showNotification()` -> can't remove or
  update it later.
- A long loop with only `message()`/`cat()` for status -> the browser shows
  nothing; use `withProgress()`/`incProgress()`.
- Using `req()` when the user should be told what's wrong -> it fails
  silently; use `validate(need(...))` for a visible message.
