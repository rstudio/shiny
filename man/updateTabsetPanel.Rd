\name{updateTabsetPanel}
\alias{updateTabsetPanel}
\title{Change the selected tab on the client}
\usage{
  updateTabsetPanel(session, inputId, selected = NULL)
}
\arguments{
  \item{session}{The \code{session} object passed to
  function given to \code{shinyServer}.}

  \item{inputId}{The id of the tabset panel object.}

  \item{selected}{The name of the tab to make active.}
}
\description{
  Change the selected tab on the client
}
\examples{
\dontrun{
shinyServer(function(input, output, session) {

  observe({
    # TRUE if input$controller is even, FALSE otherwise.
    x_even <- input$controller \%\% 2 == 0

    # Change the selected tab.
    # Note that the tabsetPanel must have been created with an 'id' argument
    if (x_even) {
      updateTabsetPanel(session, "inTabset", selected = "panel2")
    } else {
      updateTabsetPanel(session, "inTabset", selected = "panel1")
    }
  })
})
}
}
\seealso{
  \code{\link{tabsetPanel}}
}

