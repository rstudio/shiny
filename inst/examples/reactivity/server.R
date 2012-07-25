library(shiny)

shinyServer(function(input, output) {
  
  # By declaring databaseInput as a reactive function we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #
  datasetInput <- reactive(function() {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # The output$caption is computed based on a reactive function that
  # returns input$caption. When the user changes the "caption" field:
  #
  #  1) This function is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive functions below don't 
  # depend on input$caption, those functions are NOT called when the 
  # input$caption changes.
  #
  output$caption <- reactive(function() {
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive function, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  #
  output$summary <- reactiveText(function() {
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # The output$view depends on both the databaseInput reactive function,
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  #
  output$view <- reactiveTable(function() {
    obs <- as.integer(input$obs)
    head(datasetInput(), n = obs)
  })
})
