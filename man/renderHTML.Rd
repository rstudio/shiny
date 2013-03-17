\name{renderHTML}
\alias{renderHTML}
\title{HTML Output}
\usage{
  renderHTML(expr, env = parent.frame(), quoted = FALSE,
    func = NULL)
}
\arguments{
  \item{expr}{An expression that returns an R object
  representing raw HTML code. This can be a
  \code{character} vector or an \code{XMLInternalNode}.
  This object will be passed to either \code{cat} or
  \code{print} depending on its class from within a
  \code{\link[utils]{capture.output}} call.}

  \item{env}{The environment in which to evaluate
  \code{expr}.}

  \item{quoted}{Is \code{expr} a quoted expression (with
  \code{quote()})? This is useful if you want to save an
  expression in a variable.}

  \item{func}{A function that returns an object
  representing raw HTML code or a list of such objects
  (deprecated; use \code{expr} instead).}
}
\description{
  Makes a reactive version of the given function which can
  handle both \code{character} and \code{XMLInernalNode}
  results.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{shiny-text-output}.

  The result of evaluating \code{expr} is assumed to be
  HTML code and will be inserted directly into the
  corresponding output element.
}
\note{
  If the suggested \code{XML} package is not installed,
  this function is identical to \code{renderText}
}
\seealso{
  \code{\link{renderText}}
}

