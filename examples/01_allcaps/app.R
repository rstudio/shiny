library(shiny)

app <- function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
}

runApp(client='./www', server=app)