\name{reactiveUI}
\alias{reactiveUI}
\title{UI Output}
\usage{
  reactiveUI(func)
}
\arguments{
  \item{func}{A function that returns a Shiny tag object,
  \code{\link{HTML}}, or a list of such objects.}
}
\description{
  \bold{Experimental feature.} Makes a reactive version of
  a function that generates HTML using the Shiny UI
  library.
}
\details{
  The corresponding HTML output tag should be \code{div}
  and have the CSS class name \code{shiny-html-output} (or
  use \code{\link{uiOutput}}).
}
\examples{
\dontrun{
  output$moreControls <- reactiveUI(function() {
    list(

    )
  })
}
}
\seealso{
  conditionalPanel
}

