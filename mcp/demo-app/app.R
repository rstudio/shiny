library(shiny)

options(shiny.mcp = TRUE)

shinyApp(
  ui = fluidPage(
    titlePanel("MCP demo"),
    sliderInput("n", "Observations", 10, 500, 100),
    plotOutput("hist"),
    textOutput("txt")
  ),
  server = function(input, output) {
    output$hist <- renderPlot(hist(rnorm(input$n), col = "steelblue"))
    output$txt <- renderText(paste("n =", input$n))
  }
)
