library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session) {
}

opts <- list(
  port = as.numeric(readLines(tempdir(), "shiny_testthat_port", "app")),
  launch.browser = FALSE
)

shinyApp(ui, server, options = opts)
