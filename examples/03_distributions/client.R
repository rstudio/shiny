

library(shiny)

clientPage(
  applicationUI(
    
    headerPanel("Example 3: Distributions"),
    
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
      verbatimTextOutput("summary1"),
      plotOutput("plot1"),
      tableOutput("table1")
    )
  )
)
