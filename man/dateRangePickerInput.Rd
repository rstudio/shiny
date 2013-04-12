\name{dateRangePickerInput}
\alias{dateRangePickerInput}
\title{Create date range picker input}
\usage{
  dateRangePickerInput(inputId, label, start = Sys.Date(),
    end = Sys.Date(), min = NULL, max = NULL,
    format = "yyyy-MM-dd", sep = " - ", dropdowns = TRUE)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{start}{Start date.}

  \item{end}{End date.}

  \item{min}{Minimum date available.}

  \item{min}{Maximum date available.}

  \item{format}{The format of the date. Defaults to
  \code{"yyyy-MM-dd"}. Note that this differs from the date
  format string for \code{\link{datePickerInput}} in that
  the "MM" is capitalized. This is because it uses a
  different date library on the Javascript end.}

  \item{sep}{String to use as a separator between the start
  and end date.}

  \item{dropdowns}{Should the month and year be shown with
  dropdown menus for quicker navigation?}
}
\description{
  Creates a text input which, when clicked on, brings up a
  calendar that the user can click on to select start and
  end dates
}
\examples{
dateRangePickerInput("date", "Date:", value = "2012-02-29",
)
}
\seealso{
  \code{\link{updateDatePickerInput}}
}

