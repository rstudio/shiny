#' Change the value of a text input on the client
#'
#' @template update-input
#' @param value The value to set for the input object.
#'
#' @seealso \code{\link{textInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # This will change the value of input$inText, based on x
#'     updateTextInput(session, "inText", value = paste("New text", x))
#'
#'     # Can also set the label, this time for input$inText2
#'     updateTextInput(session, "inText2",
#'       label = paste("New label", x),
#'       value = paste("New text", x))
#'   })
#' })
#' }
#' @export
updateTextInput <- function(session, inputId, label = NULL, value = NULL) {
  message <- dropNulls(list(label=label, value=value))
  session$sendInputMessage(inputId, message)
}


#' Change the value of a checkbox input on the client
#'
#' @template update-input
#' @param value The value to set for the input object.
#'
#' @seealso \code{\link{checkboxInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # TRUE if input$controller is even, FALSE otherwise.
#'     x_even <- input$controller %% 2 == 0
#'
#'     updateCheckboxInput(session, "inCheckbox", value = x_even)
#'   })
#' })
#' }
#' @export
updateCheckboxInput <- updateTextInput


#' Change the value of a slider input on the client
#'
#' @template update-input
#' @param value The value to set for the input object.
#'
#' @seealso \code{\link{sliderInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # Similar to number and text. only label and value can be set for slider
#'     updateSliderInput(session, "inSlider",
#'       label = paste("Slider label", x),
#'       value = x)
#'
#'     # For sliders that pick out a range, pass in a vector of 2 values.
#'     updateSliderInput(session, "inSlider2", value = c(x-1, x+1))
#'
#'     # An NA means to not change that value (the low or high one)
#'     updateSliderInput(session, "inSlider3", value = c(NA, x+2))
#'   })
#' })
#' }
#' @export
updateSliderInput <- updateTextInput

#' Change the value of a date input on the client
#'
#' @template update-input
#' @param value The desired date value. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param min The minimum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param max The maximum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#'
#' @seealso \code{\link{dateInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     updateDateInput(session, "inDate",
#'       label = paste("Date label", x),
#'       value = paste("2013-04-", x, sep=""),
#'       min   = paste("2013-04-", x-1, sep=""),
#'       max   = paste("2013-04-", x+1, sep="")
#'     )
#'   })
#' })
#' }
#' @export
updateDateInput <- function(session, inputId, label = NULL, value = NULL,
    min = NULL, max = NULL) {

  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min, "Date"))    min   <- format(min,   "%Y-%m-%d")
  if (inherits(max, "Date"))    max   <- format(max,   "%Y-%m-%d")

  message <- dropNulls(list(label=label, value=value, min=min, max=max))
  session$sendInputMessage(inputId, message)
}


#' Change the start and end values of a date range input on the client
#'
#' @template update-input
#' @param start The start date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param end The end date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param min The minimum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#' @param max The maximum allowed date. Either a Date object, or a string in
#'   \code{yyyy-mm-dd} format.
#'
#' @seealso \code{\link{dateRangeInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     updateDateRangeInput(session, "inDateRange",
#'       label = paste("Date range label", x),
#'       start = paste("2013-01-", x, sep=""))
#'       end = paste("2013-12-", x, sep=""))
#'   })
#' })
#' }
#' @export
updateDateRangeInput <- function(session, inputId, label = NULL,
    start = NULL, end = NULL, min = NULL, max = NULL) {

  # Make sure start and end are strings, not date objects. This is for
  # consistency across different locales.
  if (inherits(start, "Date"))  start <- format(start, '%Y-%m-%d')
  if (inherits(end, "Date"))    end <- format(end, '%Y-%m-%d')
  if (inherits(min, "Date"))    min <- format(min, '%Y-%m-%d')
  if (inherits(max, "Date"))    max <- format(max, '%Y-%m-%d')

  message <- dropNulls(list(
    label = label,
    value = c(start, end),
    min = min,
    max = max
  ))

  session$sendInputMessage(inputId, message)
}

#' Change the selected tab on the client
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the tabset panel object.
#' @param selected The name of the tab to make active.
#'
#' @seealso \code{\link{tabsetPanel}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # TRUE if input$controller is even, FALSE otherwise.
#'     x_even <- input$controller %% 2 == 0
#'
#'     # Change the selected tab.
#'     # Note that the tabsetPanel must have been created with an 'id' argument
#'     if (x_even) {
#'       updateTabsetPanel(session, "inTabset", selected = "panel2")
#'     } else {
#'       updateTabsetPanel(session, "inTabset", selected = "panel1")
#'     }
#'   })
#' })
#' }
#' @export
updateTabsetPanel <- function(session, inputId, selected = NULL) {
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
}


#' Change the value of a number input on the client
#'
#' @template update-input
#' @param value The value to set for the input object.
#' @param min Minimum value.
#' @param max Maximum value.
#' @param step Step size.
#'
#' @seealso \code{\link{numericInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     updateNumericInput(session, "inNumber", value = x)
#'
#'     updateNumericInput(session, "inNumber2",
#'       label = paste("Number label ", x),
#'       value = x, min = x-10, max = x+10, step = 5)
#'   })
#' })
#' }
#' @export
updateNumericInput <- function(session, inputId, label = NULL, value = NULL,
    min = NULL, max = NULL, step = NULL) {

  message <- dropNulls(list(label=label, value=value, min=min, max=max, step=step))
  session$sendInputMessage(inputId, message)
}


#' Change the value of a checkbox group input on the client
#'
#' @template update-input
#' @param choices A named vector or named list of options. For each item, the
#'   name will be used as the label, and the value will be used as the value.
#' @param selected A vector or list of options which will be selected.
#'
#' @seealso \code{\link{checkboxGroupInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # Create a list of new options, where the name of the items is something
#'     # like 'option label x 1', and the values are 'option-x-1'.
#'     cb_options <- list()
#'     cb_options[[sprintf("option label %d 1", x)]] <- sprintf("option-%d-1", x)
#'     cb_options[[sprintf("option label %d 2", x)]] <- sprintf("option-%d-2", x)
#'
#'     # Change values for input$inCheckboxGroup
#'     updateCheckboxGroupInput(session, "inCheckboxGroup", choices = cb_options)
#'
#'     # Can also set the label and select items
#'     updateCheckboxGroupInput(session, "inCheckboxGroup2",
#'       label = paste("checkboxgroup label", x),
#'       choices = cb_options,
#'       selected = sprintf("option label %d 2", x)
#'     )
#'   })
#' })
#' }
#' @export
updateCheckboxGroupInput <- function(session, inputId, label = NULL,
  choices = NULL, selected = NULL) {

  choices <- choicesWithNames(choices)

  options <- mapply(choices, names(choices),
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = function(value, name) {
      list(value = value,
           label = name,
           checked = name %in% selected)
    }
  )

  message <- dropNulls(list(label = label, options = options))

  session$sendInputMessage(inputId, message)
}


#' Change the value of a radio input on the client
#'
#' @template update-input
#' @param choices A named vector or named list of options. For each item, the
#'   name will be used as the label, and the value will be used as the value.
#' @param selected A vector or list of options which will be selected.
#'
#' @seealso \code{\link{radioButtons}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     r_options <- list()
#'     r_options[[sprintf("option label %d 1", x)]] <- sprintf("option-%d-1", x)
#'     r_options[[sprintf("option label %d 2", x)]] <- sprintf("option-%d-2", x)
#'
#'     # Change values for input$inRadio
#'     updateRadioButtons(session, "inRadio", choices = r_options)
#'
#'     # Can also set the label and select an item
#'     updateRadioButtons(session, "inRadio2",
#'       label = paste("Radio label", x),
#'       choices = r_options,
#'       selected = sprintf("option label %d 2", x)
#'     )
#'   })
#' })
#' }
#' @export
updateRadioButtons <- updateCheckboxGroupInput


#' Change the value of a select input on the client
#'
#' @template update-input
#' @param choices A named vector or named list of options. For each item, the
#'   name will be used as the label, and the value will be used as the value.
#' @param selected A vector or list of options which will be selected.
#'
#' @seealso \code{\link{selectInput}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # We'll use the input$controller variable multiple times, so save it as x
#'     # for convenience.
#'     x <- input$controller
#'
#'     # Create a list of new options, where the name of the items is something
#'     # like 'option label x 1', and the values are 'option-x-1'.
#'     s_options <- list()
#'     s_options[[sprintf("option label %d 1", x)]] <- sprintf("option-%d-1", x)
#'     s_options[[sprintf("option label %d 2", x)]] <- sprintf("option-%d-2", x)
#'
#'     # Change values for input$inSelect
#'     updateSelectInput(session, "inSelect", choices = s_options)
#'
#'     # Can also set the label and select an item (or more than one if it's a
#'     # multi-select)
#'     updateSelectInput(session, "inSelect2",
#'       label = paste("Select label", x),
#'       choices = s_options,
#'       selected = sprintf("option label %d 2", x)
#'     )
#'   })
#' })
#' }
#' @export
updateSelectInput <- function(session, inputId, label = NULL, choices = NULL,
    selected = NULL) {

  choices <- choicesWithNames(choices)

  options <- mapply(choices, names(choices),
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = function(value, name) {
      list(value = value,
           label = name,
           selected = name %in% selected)
    }
  )

  message <- dropNulls(list(label = label, options = options))

  session$sendInputMessage(inputId, message)
}
