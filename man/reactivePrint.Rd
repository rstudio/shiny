\name{reactivePrint}
\alias{reactivePrint}
\title{Printable Output}
\usage{
  reactivePrint(func)
}
\arguments{
  \item{func}{A function that returns a printable R
  object.}
}
\description{
  Makes a reactive version of the given function that also
  turns its printable result into a string. The reactive
  function is suitable for assigning to an \code{output}
  slot.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{shiny-text-output}.

  The result of executing \code{func} will be printed
  inside a \code{\link[utils]{capture.output}} call.
}

