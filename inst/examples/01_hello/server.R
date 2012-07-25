library(shiny)

shinyServer(function(input, output) {
   
  output$plot <- reactivePlot(function() {
    obs <- as.integer(input$obs)
    hist(rnorm(obs))
  })
  
})
