library(shiny)

clientPage(
  applicationUI(
    
    headerPanel("Example 1: All Caps"),
    
    sidebarPanel(
      textInput("val", caption = "Input:", initialValue = "Hello, World!")
    ),
    
    mainPanel(
      textOutput("valUpper", caption = "You said:")
    )
  )
)

