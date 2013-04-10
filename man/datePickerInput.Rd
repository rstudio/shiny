\name{datePickerInput}
\alias{datePickerInput}
\title{Create date picker input}
\usage{
  datePickerInput(inputId, label, value = Sys.Date(),
    format = "yyyy-mm-dd")
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{value}{The starting value of the date picker.}

  \item{format}{The format of the date. Defaults to
  \code{"yyyy-mm-dd"}.}
}
\description{
  Creates a text input which, when clicked on, brings up a
  calendar that the user can click on.
}
\examples{
datePickerInput("date", "Date:", value = "2012-02-29")
}
\seealso{
  \code{\link{updateDatePickerInput}}
}

