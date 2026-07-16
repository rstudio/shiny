library(shiny)

mcpConfigure(
  appId = "demo",
  description = paste(
    "Open the interactive demo dashboard. Optionally pass `note` (shown in",
    "the app) and `n` (initial number of observations, 10-500)."
  ),
  arguments = list(
    note = ellmer::type_string("A note to show in the app"),
    n    = ellmer::type_integer("Initial observations (10-500)")
  )
)

# Additional model-callable tools that run in the server R process
registerMcpTool(
  ellmer::tool(
    function(n) {
      x <- rnorm(max(2, n))
      list(
        n = length(x),
        mean = round(mean(x), 4),
        sd = round(stats::sd(x), 4),
        range = round(range(x), 4)
      )
    },
    name = "get_sample_stats",
    description = "Summary statistics for a fresh normal sample of size n.",
    arguments = list(n = ellmer::type_integer("Sample size (>= 2)", required = TRUE))
  )
)

shinyApp(
  ui = fluidPage(
    titlePanel("MCP demo — session API"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("n", "Observations", 10, 500, 100),
        selectizeInput(
          "state", "State (server-side)",
          choices = NULL,
          options = list(placeholder = "Type to search...")
        ),
        fileInput("file", "Upload a CSV"),
        downloadButton("dl", "Download sample CSV"),
        actionButton("add", "Add dynamic panel"),
        actionButton("tell", "Tell the model about this data")
      ),
      mainPanel(
        strong(textOutput("note")),
        plotOutput("hist"),
        textOutput("txt"),
        tags$hr(),
        tableOutput("uploaded"),
        div(id = "dynamic-area")
      )
    )
  ),
  server = function(input, output, session) {
    # Arguments the model passed to the tool
    observe({
      ti <- mcpUpdates()
      if (!is.null(ti$n)) {
        updateSliderInput(session, "n", value = ti$n)
      }
    })
    output$note <- renderText({
      note <- mcpUpdates()$note
      if (is.null(note)) "" else paste("Note from the model:", note)
    })

    output$hist <- renderPlot(hist(rnorm(input$n), col = "steelblue"))
    output$txt <- renderText(paste("n =", input$n))

    # Keep the model's context up to date as the user explores
    observe({
      mcpUpdateModelContext(
        text = sprintf("The user is viewing a histogram of %d observations.", input$n),
        data = list(n = input$n)
      )
    })

    observeEvent(input$tell, {
      mcpSendMessage(sprintf(
        "Please describe what a histogram of %d draws from rnorm() typically looks like.",
        input$n
      ))
    })

    updateSelectizeInput(session, "state", choices = state.name, server = TRUE)

    output$uploaded <- renderTable({
      req(input$file)
      head(utils::read.csv(input$file$datapath), 5)
    })

    output$dl <- downloadHandler(
      filename = "sample.csv",
      content = function(file) utils::write.csv(head(mtcars, 10), file),
      contentType = "text/csv"
    )

    observeEvent(input$add, {
      insertUI(
        "#dynamic-area", "beforeEnd",
        ui = div(
          style = "padding: 8px; border: 1px solid #ccc; margin-top: 8px;",
          sprintf("Dynamic panel #%d added at %s", input$add, format(Sys.time(), "%H:%M:%S"))
        )
      )
    })
  }
)
