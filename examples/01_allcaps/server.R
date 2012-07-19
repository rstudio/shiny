library(shiny)

server(function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
})
