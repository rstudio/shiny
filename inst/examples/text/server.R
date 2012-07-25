library(shiny)

shinyServer(function(input, output) {
  
  datasetInput <- function() {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }
  
  output$summary <- reactiveText(function() {
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- reactiveTable(function() {
    obs <- as.integer(input$obs)
    head(datasetInput(), n = obs)
  })
})
