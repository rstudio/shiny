\name{numericInput}
\alias{numericInput}
\title{Create a numeric input control}
\usage{
  numericInput(inputId, label, value, min = NA, max = NA,
    step = NA)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to}

  \item{label}{Display label for the control}

  \item{value}{Initial value}

  \item{min}{Minimum allowed value}

  \item{max}{Maximum allowed value}

  \item{step}{Interval to use when stepping between min and
  max}
}
\value{
  A numeric input control that can be added to a UI
  definition.
}
\description{
  Create an input control for entry of numeric values
}
\examples{
numericInput("obs", "Observations:", 10,
             min = 1, max = 100)
}

