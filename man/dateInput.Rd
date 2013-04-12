\name{dateInput}
\alias{dateInput}
\title{Create date input}
\usage{
  dateInput(inputId, label, value = Sys.Date(),
    format = "yyyy-mm-dd")
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{value}{The starting date.}

  \item{format}{The format of the date. Defaults to
  \code{"yyyy-mm-dd"}.}
}
\description{
  Creates a text input which, when clicked on, brings up a
  calendar that the user can click on.
}
\examples{
dateInput("date", "Date:", value = "2012-02-29")
}
\seealso{
  \code{\link{updateDateInput}}
}

