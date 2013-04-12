\name{updateDateInput}
\alias{updateDateInput}
\title{Change the value of a date input on the client}
\usage{
  updateDateInput(session, inputId, label = NULL,
    value = NULL)
}
\arguments{
  \item{session}{The \code{session} object passed to
  function given to \code{shinyServer}.}

  \item{inputId}{The id of the input object.}

  \item{label}{The label to set for the input object.}

  \item{value}{The value to set for the input object.}
}
\description{
  Change the value of a date input on the client
}
\details{
  The input updater functions send a message to the client,
  telling it to change the settings of an input object. The
  messages are collected and sent after all the observers
  (including outputs) have finished running.

  The syntax of these functions is similar to the functions
  that created the inputs in the first place. For example,
  \code{\link{numericInput}()} and
  \code{updateNumericInput()} take a similar set of
  arguments.
}
\examples{
\dontrun{
shinyServer(function(input, output, clientData, session) {

  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$controller

    updateDateInput(session, "inDate",
      label = paste("Date label", x),
      value = paste("2013-04-", x, sep=""))
  })
})
}
}
\seealso{
  \code{\link{dateInput}}
}

