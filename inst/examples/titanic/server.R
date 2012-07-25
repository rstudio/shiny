library(shiny)
library(datasets)

shinyServer(function(input, output) {
  
  formulaText <- reactive(function() {
    paste("Freq ~", input$variable)
  })
  
  output$caption <- reactive(function() {
    formulaText()
  })
  
  output$plot <- reactivePlot(function() {
    boxplot(as.formula(formulaText()), 
            data = Titanic,
            outline = input$outliers)
  })
})