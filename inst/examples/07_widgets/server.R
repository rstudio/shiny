library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations. Show an alert warning if more
  # than 25 observations are requested
  output$view <- renderTable({
    
    # determine the number of observations and warn if there are
    # more than 25 requested
    n = input$obs
    if (n > 25)
      output$alert <- renderUI(alertPanel("That's a lot of observations!"))
    
    # return the requested number of observations
    head(datasetInput(), n)
  })
})
