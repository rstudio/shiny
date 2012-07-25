library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(
    h1("Shiny text")
  ),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  # Main output panel includes a summary of the dataset and an HTL
  # table with the requested number of observations
  mainPanel(
    verbatimTextOutput("summary"),
    
    tableOutput("view")
  )
))
