library(shiny)
library(datasets)

shinyUI(pageWithSidebar(
  
  headerPanel(
    h1("Titanic")
  ),
  
  sidebarPanel(
    selectInput("variable", "Variable:",
                c("Class", "Sex", "Age", "Survived")),
    
    checkboxInput("outliers", "Show outliers", FALSE)
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot")
  )
))


