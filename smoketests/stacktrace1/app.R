library(shiny)

badfunc <- function() {
  stop("boom")
}

A <- reactive({
  badfunc()
})

B <- reactive({
  A()
})

C <- reactive({
  B()
})

ui <- fluidPage(
  plotOutput("foo")
)

server <- function(input, output, session) {
  session$onFlushed(stopApp)

  output$foo <- renderPlot({
    C()
  })
}

shinyApp(ui, server)
