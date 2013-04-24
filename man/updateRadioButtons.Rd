\name{updateRadioButtons}
\alias{updateRadioButtons}
\title{Change the value of a radio input on the client}
\usage{
  updateRadioButtons(session, inputId, label = NULL,
    choices = NULL, selected = NULL)
}
\arguments{
  \item{session}{The \code{session} object passed to
  function given to \code{shinyServer}.}

  \item{inputId}{The id of the input object.}

  \item{label}{The label to set for the input object.}

  \item{choices}{A named vector or named list of options.
  For each item, the name will be used as the label, and
  the value will be used as the value.}

  \item{selected}{A vector or list of options which will be
  selected.}
}
\description{
  Change the value of a radio input on the client
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

  Any arguments with NULL values will be ignored; they will
  not result in any changes to the input object on the
  client.
}
\examples{
\dontrun{
shinyServer(function(input, output, session) {

  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$controller

    r_options <- list()
    r_options[[sprintf("option label \%d 1", x)]] <- sprintf("option-\%d-1", x)
    r_options[[sprintf("option label \%d 2", x)]] <- sprintf("option-\%d-2", x)

    # Change values for input$inRadio
    updateRadioButtons(session, "inRadio", choices = r_options)

    # Can also set the label and select an item
    updateRadioButtons(session, "inRadio2",
      label = paste("Radio label", x),
      choices = r_options,
      selected = sprintf("option label \%d 2", x)
    )
  })
})
}
}
\seealso{
  \code{\link{radioButtons}}
}

