library(shiny)

shinyUI(
  applicationPage(
    
    headerPanel("Example 1: All Caps"),
    
    sidebarPanel(
      textInput("val", label = "Input:", value = "Hello, World!")
    ),
    
    mainPanel(
      textOutput("valUpper", label = "You said:")
    )
  )
)

