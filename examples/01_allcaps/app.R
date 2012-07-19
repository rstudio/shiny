library(shiny)

client <- clientPage(
    
  textInput("val", caption = "Input:", initialValue = "Hello, World!"),
  
  textOutput("valUpper", caption = "You said:")
)

server <- function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
}

runApp(client, server, port = 8300)
