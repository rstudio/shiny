library(shiny)

shinyServer(function(input, output) {
  
  data <- reactive(function() {  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(as.integer(input$n))
  })
  
  output$plot <- reactivePlot(function() {
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  output$summary <- reactiveText(function() {
    summary(data())
  })
  
  output$table <- reactiveTable(function() {
    data.frame(x=data())
  })
  
})
