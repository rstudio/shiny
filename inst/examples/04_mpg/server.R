library(shiny)
library(datasets)

shinyServer(function(input, output) {
  
  formulaText <- reactive(function() {
    paste("mpg ~", input$variable)
  })
  
  mpgData <- reactive(function() {
    data <- mtcars
    data$cyl <- as.factor(data$cyl)
    data$am <- factor(data$am, labels = c("Automatic", "Manual"))
    data
  })
  
  output$caption <- reactive(function() {
    formulaText()
  })
  
  output$plot <- reactivePlot(function() {
    boxplot(as.formula(formulaText()), 
            data = mpgData(),
            outline = input$outliers)
  })
})
