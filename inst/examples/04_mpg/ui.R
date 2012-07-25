library(shiny)
library(datasets)

shinyUI(pageWithSidebar(
  
  headerPanel(
    h1("Miles Per Gallon")
  ),
  
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Cylinders" = "cyl", 
                     "Transmission" = "am", 
                     "Gears" = "gear")),
    
    checkboxInput("outliers", "Show outliers", FALSE)
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot")
  )
))
