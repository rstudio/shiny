library(shiny)

shinyUI(pageWithSidebar(
  
    headerPanel(
      h1("Hello shiny!")
    ),
    
    sidebarPanel(
      sliderInput("obs", 
                   "Number of observations:", 
                   min = 0, 
                   max = 1000, 
                   value = 20)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
))
