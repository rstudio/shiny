library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session) {
}

opts <- list(
  port = as.numeric(Sys.getenv("SHINY_TESTTHAT_PORT_APP", "8080")),
  launch.browser = FALSE
)

shinyApp(ui, server, options = opts)
