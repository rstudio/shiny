library(shiny)

op <- options(shiny.port = as.numeric(Sys.getenv("SHINY_TESTTHAT_PORT_OPTION", "8080")))
onStop(function() { options(op) })

ui <- fluidPage(

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
