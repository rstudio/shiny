#' Create a textarea input control
#'
#' Create a textarea input control for entry of unstructured text values.
#'
#' @inheritParams textInput
#' @param height The height of the input, e.g. `'400px'`, or
#'   `'100%'`; see \code{\link{validateCssUnit}}.
#' @param cols Value of the visible character columns of the input, e.g.
#'   `80`. If used with `width`, `width` will take precedence in
#'   the browser's rendering.
#' @param rows The value of the visible character rows of the input, e.g.
#'   `6`. If used with `height`, `height` will take precedence in
#'   the browser's rendering.
#' @param resize Which directions the textarea box can be resized. Can be one of
#'   `"both"`, `"none"`, `"vertical"`, and `"horizontal"`.
#'   The default, `NULL`, will use the client browser's default setting for
#'   resizing textareas.
#' @return A textarea input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateTextAreaInput}}
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
