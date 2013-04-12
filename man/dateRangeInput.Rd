\name{dateRangeInput}
\alias{dateRangeInput}
\title{Create date range input}
\usage{
  dateRangeInput(inputId, label, start = Sys.Date(),
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
  format string for \code{\link{dateInput}} in that the
  "MM" is capitalized. This is because it uses a different
  date library on the Javascript end.}

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
dateRangeInput("daterange", "Date range:",
               start = "2001-01-01",
               end   = "2010-12-31")

dateRangeInput("daterange", "Date range:",
               start = "2001-01-01",
               end   = "2010-12-31",
               min   = "2001-01-01",
               max   = "2012-12-21",
               format = "MM/dd/yyyy", sep = " to ")
}
\seealso{
  \code{\link{updateDateInput}}
}

