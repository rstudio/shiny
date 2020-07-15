library(shiny)

op <- options(shiny.port = 7777)
onStop(function() { options(op) })

ui <- fluidPage(

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
