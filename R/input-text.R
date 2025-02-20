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
#' @param ... Ignored, included to require named arguments and for future
#'   feature expansion.
#' @param updateOn A character vector specifying when the input should be
#'   updated. Options are `"change"` (default) and `"blur"`. Use `"change"` to
#'   update the input immediately whenever the value changes. Use `"blur"`to
#'   delay the input update until the input loses focus (the user moves away
#'   from the input), or when Enter is pressed (or Cmd/Ctrl + Enter for
#'   [inputTextArea()]).
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
  placeholder = NULL, ..., updateOn = c("change", "blur")) {    
  
  rlang::check_dots_empty()
  updateOn <- rlang::arg_match(updateOn)

  value <- restoreInput(id = inputId, default = value)

  div(class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    tags$input(id = inputId, type="text", class="shiny-input-text form-control", value=value,
      placeholder = placeholder, `data-update-on` = updateOn)
  )
}
