

library(shiny)

shinyUI(
  applicationPage(
    
    headerPanel(
      h1("Example 3: Distributions")
    ),
    
    sidebarPanel(
      selectListInput("dist", "Distibution type:",
                      list(norm = "Normal",
                           unif = "Uniform",
                           lnorm = "Log-normal",
                           exp = "Exponential")),
     
      numericInput("n", 
                   label = "Number of observations:", 
                   min = 0, 
                   max = 10000, 
                   value = 500),
              
      checkboxInput("animate", "Animate")
    ),
    
    mainPanel(
      tabset(
        tab("Plot", plotOutput("plot1")),
        tab("Summary", verbatimTextOutput("summary1")), 
        tab("Table", tableOutput("table1"))
      )
    )
  )
)
