library(shiny)


client <- clientUI(
  h1("Example One: All Caps"),
  p(
    "Input:", br(),
    input(name='val', type='text', value='Hello World!')
  ),
  p(
    "You said:", br(),
    shinyText("valUpper")
  )
)

server <- function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
}

runApp(client, server, port = 8300)
