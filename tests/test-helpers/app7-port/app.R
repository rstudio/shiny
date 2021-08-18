library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session) {
}

opts <- list(
  port = Sys.getenv(
    "SHINY_TEST_PORT_APP",
    stop("`Sys.env('SHINY_TEST_PORT_APP')` not found")
  ),
  launch.browser = FALSE
)

shinyApp(ui, server, options = opts)
