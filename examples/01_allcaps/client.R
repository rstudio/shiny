library(shiny)

clientPage(
  textInput("val", caption = "Input:", initialValue = "Hello, World!"),
  textOutput("valUpper", caption = "You said:")
)
