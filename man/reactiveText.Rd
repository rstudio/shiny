\name{reactiveText}
\alias{reactiveText}
\title{Text Output}
\usage{
  reactiveText(func)
}
\arguments{
  \item{func}{A function that returns an R object that can
  be used as an argument to \code{cat}.}
}
\description{
  Makes a reactive version of the given function that also
  uses \code{\link[base]{cat}} to turn its result into a
  single-element character vector.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{shiny-text-output}.

  The result of executing \code{func} will passed to
  \code{cat}, inside a \code{\link[utils]{capture.output}}
  call.
}

