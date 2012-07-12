# Create a timer that operates on a 1-second interval
animTimer <- reactiveTimer(1000)

data <- reactive(function() {
  if (input$animate) {
    # Invoking the timer from within this reactive function causes
    # this function to be re-run whenever the timer fires
    animTimer()
  }
  
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
}, width=600, height=300)

output$table1 <- reactiveTable(function() {
  data.frame(x=data())
})

output$summary1 <- reactiveText(function() {
  summary(data())
})
