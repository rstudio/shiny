library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
   
  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs changes.
  #  2) It's output type is a plot 
  #
  output$distPlot <- reactivePlot(function() {
    
    # convert string input to an integer
    obs <- as.integer(input$obs)
    
    # generate an rnorm distribution and plot it
    dist <- rnorm(obs)
    hist(dist)
  })
  
})
