

library(shiny)

shinyUI(
  pageWithSidebar(
    
    headerPanel(
      h1("Example 3: Distributions")
    ),
    
    sidebarPanel(
      selectInput("dist", "Distibution type:",
                  list("Normal" = "norm",
                       "Uniform" = "unif",
                       "Log-normal" = "lnorm",
                       "Exponential" = "exp")),
      
      helpText("You can select any distribution which you'd like to"),
     
      numericInput("n", 
                   "Number of observations:", 
                   min = 0, 
                   max = 10000, 
                   value = 500),
              
      checkboxInput("animate", "Animate"),
      
      submitButton()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot1")),
        tabPanel("Summary", verbatimTextOutput("summary1")), 
        tabPanel("Table", tableOutput("table1"))
      )
    )
  )
)
