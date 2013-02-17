\name{renderUI}
\alias{renderUI}
\title{UI Output}
\usage{
  renderUI(expr, env = parent.frame(), quoted = FALSE,
    func = NULL)
}
\arguments{
  \item{expr}{An expression that returns a Shiny tag
  object, \code{\link{HTML}}, or a list of such objects.}

  \item{env}{The environment in which to evaluate
  \code{expr}.}

  \item{quoted}{Is \code{expr} a quoted expression (with
  \code{quote()})? This is useful if you want to save an
  expression in a variable.}

  \item{func}{A function that returns a Shiny tag object,
  \code{\link{HTML}}, or a list of such objects
  (deprecated; use \code{expr} instead).}
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
  output$moreControls <- renderUI({
    list(

    )
  })
}
}
\seealso{
  conditionalPanel
}

