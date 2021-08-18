library(shiny)

op <- options(shiny.port = Sys.getenv(
  "SHINY_TEST_PORT_OPTION",
  stop("`Sys.env('SHINY_TEST_PORT_OPTION')` not found")
))
onStop(function() { options(op) })

ui <- fluidPage(

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
