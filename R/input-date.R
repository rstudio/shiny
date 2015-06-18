#' Create date input
#'
#' Creates a text input which, when clicked on, brings up a calendar that
#' the user can click on to select dates.
#'
#' The date \code{format} string specifies how the date will be displayed in
#' the browser. It allows the following values:
#'
#' \itemize{
#'   \item \code{yy} Year without century (12)
#'   \item \code{yyyy} Year with century (2012)
#'   \item \code{mm} Month number, with leading zero (01-12)
#'   \item \code{m} Month number, without leading zero (01-12)
#'   \item \code{M} Abbreviated month name
#'   \item \code{MM} Full month name
#'   \item \code{dd} Day of month with leading zero
#'   \item \code{d} Day of month without leading zero
#'   \item \code{D} Abbreviated weekday name
#'   \item \code{DD} Full weekday name
#' }
#'
#' @inheritParams textInput
#' @param value The starting date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param min The minimum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param max The maximum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param format The format of the date to display in the browser. Defaults to
#'   \code{"yyyy-mm-dd"}.
#' @param startview The date range shown when the input object is first
#'   clicked. Can be "month" (the default), "year", or "decade".
#' @param weekstart Which day is the start of the week. Should be an integer
#'   from 0 (Sunday) to 6 (Saturday).
#' @param language The language used for month and day names. Default is "en".
#'   Other valid values include "bg", "ca", "cs", "da", "de", "el", "es", "fi",
#'   "fr", "he", "hr", "hu", "id", "is", "it", "ja", "kr", "lt", "lv", "ms",
#'   "nb", "nl", "pl", "pt", "pt-BR", "ro", "rs", "rs-latin", "ru", "sk", "sl",
#'   "sv", "sw", "th", "tr", "uk", "zh-CN", and "zh-TW".
#'
#' @family input elements
#' @seealso \code{\link{dateRangeInput}}, \code{\link{updateDateInput}}
#'
#' @examples
#' dateInput("date", "Date:", value = "2012-02-29")
#'
#' # Default value is the date in client's time zone
#' dateInput("date", "Date:")
#'
#' # value is always yyyy-mm-dd, even if the display format is different
#' dateInput("date", "Date:", value = "2012-02-29", format = "mm/dd/yy")
#'
#' # Pass in a Date object
#' dateInput("date", "Date:", value = Sys.Date()-10)
#'
#' # Use different language and different first day of week
#' dateInput("date", "Date:",
#'           language = "de",
#'           weekstart = 1)
#'
#' # Start with decade view instead of default month view
#' dateInput("date", "Date:",
#'           startview = "decade")
#'
#' @export
dateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
  format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en",
  width = NULL) {

  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")

  attachDependencies(
    tags$div(id = inputId,
      class = "shiny-date-input form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

      controlLabel(inputId, label),
      tags$input(type = "text",
                 # datepicker class necessary for dropdown to display correctly
                 class = "form-control datepicker",
                 `data-date-language` = language,
                 `data-date-weekstart` = weekstart,
                 `data-date-format` = format,
                 `data-date-start-view` = startview,
                 `data-min-date` = min,
                 `data-max-date` = max,
                 `data-initial-date` = value
      )
    ),
    datePickerDependency
  )
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.0.2", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css")
