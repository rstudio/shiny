library(shiny)

client <- clientPage(
    
  textInput("val", 
            label = "Input:", 
            value = "Hello, World!",
            labelOnTop = TRUE),
  
  p("You said:"), shinyText("valUpper")
)

server <- function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
}

runApp(client, server, port = 8300)
