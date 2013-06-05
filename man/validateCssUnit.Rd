\name{validateCssUnit}
\alias{validateCssUnit}
\title{Validate proper CSS formatting of a unit}
\usage{
  validateCssUnit(x)
}
\arguments{
  \item{x}{The unit to validate. Will be treated as a
  number of pixels if a unit is not specified.}
}
\value{
  A properly formatted CSS unit of length, if possible.
  Otherwise, will throw an error.
}
\description{
  Validate proper CSS formatting of a unit
}
\examples{
validateCssUnit("10\%")
validateCssUnit(400)  #treated as '400px'
}

