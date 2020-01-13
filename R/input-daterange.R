#' Create date range input
#'
#' Creates a pair of text inputs which, when clicked on, bring up calendars that
#' the user can click on to select dates.
#'
#' The date `format` string specifies how the date will be displayed in
#' the browser. It allows the following values:
#'
#' \itemize{
#'   \item `yy` Year without century (12)
#'   \item `yyyy` Year with century (2012)
#'   \item `mm` Month number, with leading zero (01-12)
#'   \item `m` Month number, without leading zero (1-12)
#'   \item `M` Abbreviated month name
#'   \item `MM` Full month name
#'   \item `dd` Day of month with leading zero
#'   \item `d` Day of month without leading zero
#'   \item `D` Abbreviated weekday name
#'   \item `DD` Full weekday name
#' }
#'
#' @inheritParams dateInput
#' @param start The initial start date. Either a Date object, or a string in
#'   `yyyy-mm-dd` format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param end The initial end date. Either a Date object, or a string in
#'   `yyyy-mm-dd` format. If NULL (the default), will use the current
#'   date in the client's time zone.
#' @param separator String to display between the start and end input boxes.
#'
#' @family input elements
#' @seealso [dateInput()], [updateDateRangeInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   dateRangeInput("daterange1", "Date range:",
#'                  start = "2001-01-01",
#'                  end   = "2010-12-31"),
#'
#'   # Default start and end is the current date in the client's time zone
#'   dateRangeInput("daterange2", "Date range:"),
#'
#'   # start and end are always specified in yyyy-mm-dd, even if the display
#'   # format is different
#'   dateRangeInput("daterange3", "Date range:",
#'                  start  = "2001-01-01",
#'                  end    = "2010-12-31",
#'                  min    = "2001-01-01",
#'                  max    = "2012-12-21",
#'                  format = "mm/dd/yy",
#'                  separator = " - "),
#'
#'   # Pass in Date objects
#'   dateRangeInput("daterange4", "Date range:",
#'                  start = Sys.Date()-10,
#'                  end = Sys.Date()+10),
#'
#'   # Use different language and different first day of week
#'   dateRangeInput("daterange5", "Date range:",
#'                  language = "de",
#'                  weekstart = 1),
#'
#'   # Start with decade view instead of default month view
#'   dateRangeInput("daterange6", "Date range:",
#'                  startview = "decade")
#' )
#'
#' shinyApp(ui, server = function(input, output) { })
#' }
#'
#' @section Server value:
#' A [Date] vector of length 2.
#'
#' @export
dateRangeInput <- function(inputId, label, start = NULL, end = NULL,
    min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
    weekstart = 0, language = "en", separator = " to ", width = NULL,
    autoclose = TRUE) {

  start <- dateYMD(start, "start")
  end   <- dateYMD(end, "end")
  min   <- dateYMD(min, "min")
  max   <- dateYMD(max, "max")

  restored <- restoreInput(id = inputId, default = list(start, end))
  start <- restored[[1]]
  end <- restored[[2]]

  attachDependencies(
    div(id = inputId,
      class = "shiny-date-range-input form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),

      shinyInputLabel(inputId, label),
      # input-daterange class is needed for dropdown behavior
      div(class = "input-daterange input-group input-group-sm",
        tags$input(
          class = "form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-week-start` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = start,
          `data-date-autoclose` = if (autoclose) "true" else "false"
        ),
        # input-group-prepend and input-group-append are for bootstrap 4 forward compat
        span(class = "input-group-addon input-group-prepend input-group-append",
          span(class = "input-group-text",
            separator
          )
        ),
        tags$input(
          class = "form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-week-start` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = end,
          `data-date-autoclose` = if (autoclose) "true" else "false"
        )
      )
    ),
    datePickerDependency
  )
}
