\name{validateCssUnit}
\alias{validateCssUnit}
\title{Validate proper CSS formatting of a unit}
\usage{
validateCssUnit(x)
}
\arguments{
\item{x}{The unit to validate. Will be treated as a number of pixels if a
unit is not specified.}
}
\value{
A properly formatted CSS unit of length, if possible. Otherwise, will
  throw an error.
}
\description{
Checks that the argument is valid for use as a CSS unit of length.
}
\details{
\code{NULL} and \code{NA} are returned unchanged.

Single element numeric vectors are returned as a character vector with the
number plus a suffix of \code{"px"}.

Single element character vectors must be \code{"auto"} or \code{"inherit"},
a number, or a length calculated by the \code{"calc"} CSS function.
If the number has a suffix, it must be valid: \code{px},
\code{\%}, \code{ch}, \code{em}, \code{rem}, \code{pt}, \code{in}, \code{cm},
\code{mm}, \code{ex}, \code{pc}, \code{vh}, \code{vw}, \code{vmin}, or
\code{vmax}.
If the number has no suffix, the suffix \code{"px"} is appended.


Any other value will cause an error to be thrown.
}
\examples{
validateCssUnit("10\%")
validateCssUnit(400)  #treated as '400px'
}
