#' Create a text input control
#'
#' Create an input control for entry of unstructured text values
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'   see [validateCssUnit()].
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
#' @param updateOn A character vector specifying when the input should be
#'   updated. Options are `"input"` (default) and `"blur"`. If `"blur"`, then
#'   the input value will be updated when the text input loses focus, or when
#'   Enter is pressed.
#' @param debounce The debouncing delay in milliseconds when 'input' is used.
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateTextInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   textInput("caption", "Caption", "Data Summary"),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$caption })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' A character string of the text input. The default value is `""`
#' unless `value` is provided.
#'
#' @export
textInput <- function(inputId, label, value = "", width = NULL,
  placeholder = NULL, updateOn = c("input", "blur"), debounce = 250) {

  updateOn <- match.arg(updateOn)

  value <- restoreInput(id = inputId, default = value)

  div(class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    tags$input(id = inputId, type="text", class="shiny-input-text form-control", value=value,
      placeholder = placeholder, `data-update-on` = updateOn, `data-debounce` = debounce)
  )
}
