library(shiny)

# Third demo app: a live clock that ticks every second with a pause/resume
# button. It also demonstrates fullscreen display-mode support --
# `mcpConfigure(displayModes = ...)` declares the modes the app supports and
# `mcpRequestDisplayMode()` asks the host to switch. The optional `label` and
# `paused` arguments are restored via `mcpUpdates()` when the model opens it.
mcpConfigure(
  appId = "clock",
  description = paste(
    "Open a live clock that updates every second, with a button to pause and",
    "resume the ticks. Optionally pass `label` (a heading shown above the",
    "clock) and `paused` (start paused instead of running)."
  ),
  arguments = list(
    label  = ellmer::type_string("Heading shown above the clock"),
    paused = ellmer::type_boolean("Start paused instead of running")
  ),
  # Declare that this app can render inline or take over the full window. The
  # host may still decline a request; observe mcpHostContext()$displayMode for
  # the mode actually in effect.
  displayModes = c("inline", "fullscreen")
)

shinyApp(
  ui = fluidPage(
    titlePanel(textOutput("heading", inline = TRUE)),
    div(
      style = paste(
        "font-family: monospace; font-size: 4rem; font-weight: 700;",
        "text-align: center; padding: 1.5rem 0; letter-spacing: 0.05em;"
      ),
      textOutput("clock")
    ),
    div(
      style = "text-align: center;",
      actionButton("toggle", "Pause", class = "btn-primary"),
      actionButton("fullscreen", "Go fullscreen", class = "btn-default"),
      actionButton("inline", "Back to inline", class = "btn-default")
    ),
    div(
      style = "text-align: center; color: #888; margin-top: 1rem;",
      textOutput("status")
    )
  ),
  server = function(input, output, session) {
    running <- reactiveVal(TRUE)
    heading <- reactiveVal("Live clock")

    # Restore any arguments the model passed when opening the app.
    observe({
      args <- mcpUpdates()
      if (!is.null(args$label) && nzchar(args$label)) {
        heading(args$label)
      }
      if (isTRUE(args$paused)) {
        running(FALSE)
      }
    })

    # Toggle the ticking on/off.
    observeEvent(input$toggle, {
      running(!running())
    })
    observe({
      updateActionButton(
        session, "toggle",
        label = if (running()) "Pause" else "Resume"
      )
    })

    # The clock reschedules itself every second while running; when paused it
    # stops invalidating and simply holds the last time.
    currentTime <- reactive({
      if (running()) {
        invalidateLater(1000, session)
      }
      Sys.time()
    })

    output$heading <- renderText(heading())

    output$clock <- renderText({
      format(currentTime(), "%H:%M:%S")
    })

    # Fullscreen / inline requests to the host.
    observeEvent(input$fullscreen, {
      mcpRequestDisplayMode("fullscreen")
    })
    observeEvent(input$inline, {
      mcpRequestDisplayMode("inline")
    })

    output$status <- renderText({
      ctx <- mcpHostContext()
      mode <- if (!is.null(ctx$displayMode)) ctx$displayMode else "inline"
      sprintf(
        "%s | display mode: %s",
        if (running()) "Running" else "Paused",
        mode
      )
    })

    # Keep the model aware of the clock's state.
    observe({
      mcpUpdateModelContext(
        text = sprintf(
          "The clock is currently %s.",
          if (running()) "running" else "paused"
        ),
        data = list(running = running())
      )
    })
  }
)
