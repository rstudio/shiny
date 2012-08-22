\name{checkboxInput}
\alias{checkboxInput}
\title{Checkbox Input Control}
\usage{
  checkboxInput(inputId, label, value = FALSE)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{value}{Initial value (\code{TRUE} or
  \code{FALSE}).}
}
\value{
  A checkbox control that can be added to a UI definition.
}
\description{
  Create a checkbox that can be used to specify logical
  values.
}
\examples{
checkboxInput("outliers", "Show outliers", FALSE)
}
\seealso{
  \code{\link{checkboxGroupInput}}
}

