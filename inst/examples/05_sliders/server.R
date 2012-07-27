library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive function to compose a data frame containing all of the values
  sliderValues <- reactive(function() {
    
    # Show values using R's default print format
    printValue <- function(value) {
      capture.output(print(value))
    }
    
    # Compose data frame
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
  
  # Show the values using an HTML table
  output$values <- reactiveTable(function() {
    sliderValues()
  })
})
