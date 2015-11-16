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
  textOutput("foo"),
  dataTableOutput("bar")
)

server <- function(input, output, session) {
  session$onFlushed(stopApp)

  output$foo <- renderText({
    C()
  })
  output$bar <- renderDataTable({
    C()
  })
}

shinyApp(ui, server)
