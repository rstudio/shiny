library(shiny)

# Define UI for random distribution application 
shinyUI(
  pageWithSidebar(
    
    # Application title
    headerPanel(
      h1("Tabsets")
    ),
    
    # Sidebar with controls to select the random distribution type
    # and number of observations to generate
    sidebarPanel(
      selectInput("dist", "Distribution type:",
                  list("Normal" = "norm",
                       "Uniform" = "unif",
                       "Log-normal" = "lnorm",
                       "Exponential" = "exp")),
      
      sliderInput("n", 
                  "Number of observations:", 
                   value = 500,
                   min = 1, 
                   max = 1000)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)
