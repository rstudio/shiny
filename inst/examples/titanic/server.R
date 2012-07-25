library(shiny)
library(datasets)

shinyServer(function(input, output) {
  
  model <- reactive(function() {
    paste("Freq ~", input$variable)
  })
  
  output$caption <- reactive(function() {
    model()
  })
  
  output$plot <- reactivePlot(function() {
    boxplot(as.formula(model()), 
            data = Titanic,
            outline = input$outliers)
  })
})