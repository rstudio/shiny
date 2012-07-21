library(shiny)

shinyUI(
  applicationPage(
    headerPanel(
      h1("Example 1: All Caps")
    ),
    
    sidebarPanel(
      textInput("val", "Input:", value = "Hello, World!")
    ),
    
    mainPanel(
      textOutput("valUpper", "You said:")
    )
  )
)

