library(shiny)

shinyUI(
  applicationPage(
    headerPanel(
      h1("Example 1: All Caps")
    ),
    
    sidebarPanel(
      textInput("val", label = "Input:", value = "Hello, World!")
    ),
    
    mainPanel(
      textOutput("valUpper", label = "You said:")
    )
  )
)

