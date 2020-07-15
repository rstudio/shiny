library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session) {
}

opts <- list(
  port = 3030,
  launch.browser = FALSE
)

shinyApp(ui, server, options = opts)
