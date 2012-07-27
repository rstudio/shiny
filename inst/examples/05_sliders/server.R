library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive function to compose a data frame containing all of the values
  sliderValues <- reactive(function() {
    
    # Compose data frame
    data.frame(
      Name = c("Integer", 
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer, 
                             input$decimal,
                             paste(input$range, collapse=' '),
                             input$format,
                             input$animation)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- reactiveTable(function() {
    sliderValues()
  })
})
