library(shiny)
library(datasets)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
  
  formulaText <- reactive(function() {
    paste("mpg ~", input$variable)
  })
   
  output$caption <- reactive(function() {
    formulaText()
  })
  
  output$plot <- reactivePlot(function() {
    boxplot(as.formula(formulaText()), 
            data = mpgData,
            outline = input$outliers)
  })
})
