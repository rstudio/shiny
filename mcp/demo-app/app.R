library(shiny)

options(shiny.mcp = TRUE)

shinyApp(
  ui = fluidPage(
    titlePanel("MCP demo — Phase 2"),
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
        actionButton("add", "Add dynamic panel")
      ),
      mainPanel(
        plotOutput("hist"),
        textOutput("txt"),
        tags$hr(),
        tableOutput("uploaded"),
        div(id = "dynamic-area")
      )
    )
  ),
  server = function(input, output, session) {
    output$hist <- renderPlot(hist(rnorm(input$n), col = "steelblue"))
    output$txt <- renderText(paste("n =", input$n))

    updateSelectizeInput(
      session, "state",
      choices = state.name, server = TRUE
    )

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
