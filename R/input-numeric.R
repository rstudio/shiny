#' Create a numeric input control
#'
#' Create an input control for entry of numeric values
#'
#' @inheritParams textInput
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#' @return A numeric input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateNumericInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   numericInput("obs", "Observations:", 10, min = 1, max = 100),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$obs })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' A numeric vector of length 1.
#'
#' @export
numericInput <- function(inputId, label, value, min = NA, max = NA, step = NA,
  width = NULL, ..., updateOn = c("change", "blur")) {
  rlang::check_dots_empty()
  updateOn <- match.arg(updateOn)

  value <- restoreInput(id = inputId, default = value)

  # build input tag
  inputTag <- tags$input(id = inputId, type = "number", class="shiny-input-number form-control",
                         value = formatNoSci(value), `data-update-on` = updateOn)
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step

  div(class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    inputTag
  )
}
