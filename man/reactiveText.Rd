\name{reactiveText}
\alias{reactiveText}
\title{Text Output}
\usage{
  reactiveText(func)
}
\arguments{
  \item{func}{A function that returns a printable R object,
  or an invisible character vector.}
}
\description{
  Creates a reactive text value that is suitable for
  assigning to an \code{output} slot.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{live-text}.

  The result of executing \code{func} will be printed
  inside a \code{\link[utils]{capture.output}} call. If
  instead you want to return a single-element character
  vector to be used verbatim, return it wrapped in
  \code{\link[base]{invisible}} (or alternatively, just use
  \code{\link{reactive}} instead of \code{reactiveText}.
}

