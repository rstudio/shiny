library(shiny)

shinyUI(pageWithSidebar(
  
    headerPanel(
      h1("Hello shiny!")
    ),
    
    sidebarPanel(
      numericInput("obs", 
                   "Number of observations:", 
                   min = 0, 
                   max = 10000, 
                   value = 500)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
))
