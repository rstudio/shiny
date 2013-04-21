\name{dateInput}
\alias{dateInput}
\title{Create date input}
\usage{
  dateInput(inputId, label, value = Sys.Date(), min = NULL,
    max = NULL, format = "\%Y-\%m-\%d")
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{value}{The starting date. Either a Date object, or
  a string in \code{yyyy-mm-dd} format.}

  \item{min}{The minimum allowed date. Either a Date
  object, or a string in \code{yyyy-mm-dd} format.}

  \item{max}{The maximum allowed date. Either a Date
  object, or a string in \code{yyyy-mm-dd} format.}

  \item{format}{The format of the date. Defaults to
  \code{"%Y-%m-%d"}.}
}
\description{
  Creates a text input which, when clicked on, brings up a
  calendar that the user can click on.
}
\details{
  The date \code{format} string uses a subset of R's
  \code{\link{strptime}} format codes. The reason it uses a
  subset is because the formats must be the Javascript date
  picker only supports some of the format codes. Valid
  values are:

  \itemize{ \item \code{%y} Year without century (12) \item
  \code{%Y} Year with century (2012) \item \code{%m} Month
  number, with leading zero (01-12) \item \code{%b}
  Abbreviated month name \item \code{%B} Full month name
  \item \code{%d} Day of month with leading zero \item
  \code{%e} Day of month without leading zero \item
  \code{%a} Abbreviated weekday name \item \code{%A} Full
  weekday name }
}
\examples{
dateInput("date", "Date:", value = "2012-02-29")

# value is always yyyy-mm-dd, even if the display format is different
dateInput("date", "Date:", value = "2012-02-29", format = "\%m/\%d/\%y")

# Pass in a date object
dateInput("date", "Date:", value = Sys.Date()-10)
}
\seealso{
  \code{\link{updateDateInput}}
}

