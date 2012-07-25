library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel(
    h1("Reactivity")
  ),
  
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  mainPanel(
    h3(textOutput("caption")), 
    verbatimTextOutput("summary"), 
    tableOutput("view")
  )
))
