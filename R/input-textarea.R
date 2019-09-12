#' Create a textarea input control
#'
#' Create a textarea input control for entry of unstructured text values.
#'
#' @inheritParams textInput
#' @param height The height of the input, e.g. `'400px'`, or `'100%'`; see
#'   [validateCssUnit()].
#' @param cols Value of the visible character columns of the input, e.g. `80`.
#'   This argument will only take effect if there is not a CSS `width` rule
#'   defined for this element; such a rule could come from the `width` argument
#'   of this function or from a containing page layout such as
#'   [fluidPage()].
#' @param rows The value of the visible character rows of the input, e.g. `6`.
#'   If the `height` argument is specified, `height` will take precedence in the
#'   browser's rendering.
#' @param resize Which directions the textarea box can be resized. Can be one of
#'   `"both"`, `"none"`, `"vertical"`, and `"horizontal"`. The default, `NULL`,
#'   will use the client browser's default setting for resizing textareas.
#' @return A textarea input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateTextAreaInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   textAreaInput("caption", "Caption", "Data Summary", width = "1000px"),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$caption })
#' }
#' shinyApp(ui, server)
#'
#' }
#'
#' @section Server value:
#' A character string of the text input. The default value is `""`
#' unless `value` is provided.
#'
#' @export
textAreaInput <- function(inputId, label, value = "", width = NULL, height = NULL,
  cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) {

  value <- restoreInput(id = inputId, default = value)

  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }

  style <- paste(
    if (!is.null(width))  paste0("width: ",  validateCssUnit(width),  ";"),
    if (!is.null(height)) paste0("height: ", validateCssUnit(height), ";"),
    if (!is.null(resize)) paste0("resize: ", resize, ";")
  )

  # Workaround for tag attribute=character(0) bug:
  #   https://github.com/rstudio/htmltools/issues/65
  if (length(style) == 0) style <- NULL

  div(class = "form-group shiny-input-container",
    shinyInputLabel(inputId, label),
    tags$textarea(
      id = inputId,
      class = "form-control",
      placeholder = placeholder,
      style = style,
      rows = rows,
      cols = cols,
      value
    )
  )
}
