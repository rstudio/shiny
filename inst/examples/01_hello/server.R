library(shiny)

tickFactor <- function (x) {
  calcTickSize <- function(range) {
    tickSize <- 10^(floor(log10(range*x)))
    floor(range/tickSize)
  }
  
  range <- 1:10000
  df <- data.frame(range=range, ticks=calcTickSize(range))
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  tickCounts <- reactive(function() {
    tickFactor(as.numeric(input$obs))
  })
   
  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot <- reactivePlot(function() {
    plot(tickCounts())
  })
  output$min <- reactiveText(function() {
    min(tickCounts()$ticks)
  })
  output$max <- reactiveText(function() {
    max(tickCounts()$ticks)
  })
})
