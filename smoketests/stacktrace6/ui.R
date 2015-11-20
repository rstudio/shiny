library(shiny)

baduicall <- function() {
  stopApp()
  stop("ui boom")
}

ui <- fluidPage(
  wellPanel(baduicall())
)
