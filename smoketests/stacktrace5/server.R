library(shiny)

badservercall <- function() {
  stop("server boom")
}

function(input, output, session) {
  on.exit(stopApp())
  badservercall()
}
