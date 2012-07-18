library(shiny)


ui <- defineUI(
  h1("Example 1: All Caps"),
  p(
    "Input:", br(),
    input(name='val', type='text', value='Hello World!')
  ),
  p(
    "You said:", br(),
    shinyText("valUpper")
  )
)

app <- function(input, output) {
  output$valUpper <- reactive(function() {
    toupper(input$val)
  })
}

runApp(client=page(ui), server=app)
