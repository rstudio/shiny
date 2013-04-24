\name{updateCheckboxGroupInput}
\alias{updateCheckboxGroupInput}
\title{Change the value of a checkbox group input on the client}
\usage{
  updateCheckboxGroupInput(session, inputId, label = NULL,
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
  Change the value of a checkbox group input on the client
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

    # Create a list of new options, where the name of the items is something
    # like 'option label x 1', and the values are 'option-x-1'.
    cb_options <- list()
    cb_options[[sprintf("option label \%d 1", x)]] <- sprintf("option-\%d-1", x)
    cb_options[[sprintf("option label \%d 2", x)]] <- sprintf("option-\%d-2", x)

    # Change values for input$inCheckboxGroup
    updateCheckboxGroupInput(session, "inCheckboxGroup", choices = cb_options)

    # Can also set the label and select items
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
      label = paste("checkboxgroup label", x),
      choices = cb_options,
      selected = sprintf("option label \%d 2", x)
    )
  })
})
}
}
\seealso{
  \code{\link{checkboxGroupInput}}
}

