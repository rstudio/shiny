#' Create date range input
#'
#' Creates a pair of text inputs which, when clicked on, bring up calendars that
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
#' @inheritParams dateInput
#' @param start The initial start date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param end The initial end date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param separator String to display between the start and end input boxes.
#'
#' @family input elements
#' @seealso \code{\link{dateInput}}, \code{\link{updateDateRangeInput}}
#'
#' @examples
#' dateRangeInput("daterange", "Date range:",
#'                start = "2001-01-01",
#'                end   = "2010-12-31")
#'
#' # Default start and end is the current date in the client's time zone
#' dateRangeInput("daterange", "Date range:")
#'
#' # start and end are always specified in yyyy-mm-dd, even if the display
#' # format is different
#' dateRangeInput("daterange", "Date range:",
#'                start  = "2001-01-01",
#'                end    = "2010-12-31",
#'                min    = "2001-01-01",
#'                max    = "2012-12-21",
#'                format = "mm/dd/yy",
#'                separator = " - ")
#'
#' # Pass in Date objects
#' dateRangeInput("daterange", "Date range:",
#'                start = Sys.Date()-10,
#'                end = Sys.Date()+10)
#'
#' # Use different language and different first day of week
#' dateRangeInput("daterange", "Date range:",
#'                language = "de",
#'                weekstart = 1)
#'
#' # Start with decade view instead of default month view
#' dateRangeInput("daterange", "Date range:",
#'                startview = "decade")
#'
#' @export
dateRangeInput <- function(inputId, label, start = NULL, end = NULL,
    min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
    weekstart = 0, language = "en", separator = " to ", width = NULL) {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")

  attachDependencies(
    div(id = inputId,
      class = "shiny-date-range-input form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

      controlLabel(inputId, label),
      # input-daterange class is needed for dropdown behavior
      div(class = "input-daterange input-group",
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = start
        ),
        span(class = "input-group-addon", separator),
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = end
        )
      )
    ),
    datePickerDependency
  )
}
