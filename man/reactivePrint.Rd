\name{reactivePrint}
\alias{reactivePrint}
\title{Printable Output}
\usage{
  reactivePrint(func)
}
\arguments{
  \item{func}{A function that may print output and/or
  return a printable R object.}
}
\description{
  Makes a reactive version of the given function that
  captures any printed output, and also captures its
  printable result (unless \code{\link{invisible}}), into a
  string. The resulting function is suitable for assigning
  to an \code{output} slot.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{shiny-text-output}.

  The result of executing \code{func} will be printed
  inside a \code{\link[utils]{capture.output}} call.

  Note that unlike most other Shiny output functions, if
  the given function returns \code{NULL} then \code{NULL}
  will actually be visible in the output. To display
  nothing, make your function return
  \code{\link{invisible}()}.
}

