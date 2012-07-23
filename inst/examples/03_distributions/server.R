library(shiny)

server(function(input, output) {
  
  data <- reactive(function() {
  
    # If the Animate checkbox is checked, then we want to re-calculate this
    # reactive function 1 second from now
    if (input$animate)
      invalidateLater(1000)
    
    # Choose a distribution function
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    # Generate n values from the distribution function
    dist(as.integer(input$n))
  })
  
  output$plot1 <- reactivePlot(function() {
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  output$table1 <- reactiveTable(function() {
    data.frame(x=data())
  })
  
  output$summary1 <- reactiveText(function() {
    summary(data())
  })
})
