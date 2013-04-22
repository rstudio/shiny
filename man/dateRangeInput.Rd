\name{dateRangeInput}
\alias{dateRangeInput}
\title{Create date range input}
\usage{
  dateRangeInput(inputId, label, start = Sys.Date(),
    end = Sys.Date(), min = NULL, max = NULL,
    format = "\%Y-\%m-\%d", startview = "month",
    weekstart = 0, language = "en", separator = " to ")
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{start}{The initial start date. Either a Date
  object, or a string in \code{yyyy-mm-dd} format.}

  \item{start}{The initial end date. Either a Date object,
  or a string in \code{yyyy-mm-dd} format.}

  \item{min}{The minimum allowed date. Either a Date
  object, or a string in \code{yyyy-mm-dd} format.}

  \item{max}{The maximum allowed date. Either a Date
  object, or a string in \code{yyyy-mm-dd} format.}

  \item{format}{The format of the date. Defaults to
  \code{"\%Y-\%m-\%d"}.}

  \item{startview}{The date range shown when the input
  object is first clicked. Can be "month" (the default),
  "year", or "decade".}

  \item{weekstart}{Which day is the start of the week.
  Should be an integer from 0 (Sunday) to 6 (Saturday).}

  \item{language}{The language used for month and day
  names. Default is "en".  Other valid values include "bg",
  "ca", "cs", "da", "de", "el", "es", "fi", "fr", "he",
  "hr", "hu", "id", "is", "it", "ja", "kr", "lt", "lv",
  "ms", "nb", "nl", "pl", "pt", "pt", "ro", "rs", "rs",
  "ru", "sk", "sl", "sv", "sw", "th", "tr", "uk", "zh-CN",
  and "zh-TW".}
}
\description{
  Creates a pair of text inputs which, when clicked on,
  bring up calendars that the user can click on to select
  dates.
}
\details{
  The date \code{format} string uses a subset of R's
  \code{\link{strptime}} format codes. The reason it uses a
  subset is because the formats must be the Javascript date
  picker only supports some of the format codes. Valid
  values are:

  \itemize{ \item \code{\%y} Year without century (12)
  \item \code{\%Y} Year with century (2012) \item
  \code{\%m} Month number, with leading zero (01-12) \item
  \code{\%b} Abbreviated month name \item \code{\%B} Full
  month name \item \code{\%d} Day of month with leading
  zero \item \code{\%e} Day of month without leading zero
  \item \code{\%a} Abbreviated weekday name \item
  \code{\%A} Full weekday name }
}
\examples{
dateRangeInput("daterange", "Date range:",
               start = "2001-01-01",
               end   = "2010-12-31")

# start and end are always yyyy-mm-dd, even if the display format
# is different
dateRangeInput("daterange", "Date range:",
               start  = "2001-01-01",
               end    = "2010-12-31",
               min    = "2001-01-01",
               max    = "2012-12-21",
               format = "mm/dd/yy",
               separator = " - ")

# Pass in date objects
dateRangeInput("daterange", "Date range:",
               start = Sys.Date()-10,
               end = Sys.Date()+10)

# Use different language and different first day of week
dateRangeInput("daterange", "Date range:",
               language = "de",
               weekstart = 1)

# Start with decade view instead of default month view
dateRangeInput("daterange", "Date range:",
               startview = "decade")
}
\seealso{
  \code{\link{DateInput}},
  \code{\link{updateDateRangeInput}}
}

