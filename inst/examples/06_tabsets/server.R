library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive function to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all use the value computed from this function
  data <- reactive(function() {  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive function are both tracked, and all functions 
  # are called in the sequence implied by the dependency graph
  output$plot <- reactivePlot(function() {
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- reactivePrint(function() {
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- reactiveTable(function() {
    data.frame(x=data())
  })
  
})
