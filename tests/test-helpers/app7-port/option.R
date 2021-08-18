library(shiny)

op <- options(shiny.port = as.numeric(readLines(tempdir(), "shiny_testthat_port", "option")))
onStop(function() { options(op) })

ui <- fluidPage(

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
