library(shiny)
library(future)
library(promises)
library(magrittr)
plan(multisession)

ui <- fluidPage(
  p("This app tests that ", tags$code("invalidateLater()"), " calls are held until async operations are complete."),
  tags$ol(
    tags$li("You should see the number below increasing by 1, every 2 seconds."),
    tags$li("The output should be semi-transparent (i.e. recalculating state) continuously."),
    tags$li("You should see the word 'Flushed' in the R console, every 2 seconds.")
  ),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {

  value <- reactiveVal(0L)

  observe({
    invalidateLater(100)
    isolate({ value(value() + 1L) })
  })

  session$onFlushed(function() {
    print("Flushed")
  }, once = FALSE)

  output$out <- renderText({
    future(Sys.sleep(2)) %...>%
      { value() }
  })
}

shinyApp(ui, server)
