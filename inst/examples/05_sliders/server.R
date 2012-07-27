library(shiny)




shinyServer(function(input, output) {
  

  sliderValues <- reactive(function() {
    
    printValue <- function(value) {
      capture.output(print(value))
    }
    
    data.frame(
      Name = c("Integer", 
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      
      Value = c(printValue(input$integer), 
                printValue(input$decimal),
                printValue(input$range),
                printValue(input$format),
                printValue(input$animation)),
      
      stringsAsFactors=FALSE)
  }) 
  
  output$values <- reactiveTable(function() {
    sliderValues()
  })
  
})
