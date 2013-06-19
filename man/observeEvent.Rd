\name{observeEvent}
\alias{eventFilter}
\alias{observeEvent}
\title{Event handler}
\usage{
  observeEvent(eventExpr, callback, env = parent.frame(),
    quoted = FALSE)

  eventFilter(eventExpr, valueFunc, env = parent.frame(),
    quoted = FALSE)
}
\description{
  Event handler
}
\examples{
\dontrun{
# In ui.R:
shinyUI(basicPage(
  numericInput("n", "Number of observations", 5),
  actionButton("saveButton", "Save")
))
# In server.R:
shinyServer(function(input, output) {
  handleButtonClick(input$saveButton, function() {
    write.csv(runif(input$n), file = "data.csv")
  })
})
}
}
\seealso{
  \code{\link{actionButton}}
}

