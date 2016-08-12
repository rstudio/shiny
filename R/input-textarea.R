#' Create a textarea input control
#'
#' Create a resizable input control for entry of unstructured text values
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param value Initial value.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
#'   see \code{\link{validateCssUnit}}.
#' @param height The height of the input, e.g. \code{'400px'}, or \code{'100\%'};
#'   see \code{\link{validateCssUnit}}.
#' @param cols Value of the visible character columns of the input, e.g.
#'   \code{'80'}.
#' @param rows The value of the visible character rows of the input, e.g.
#'   \code{'6'}.
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
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
#' }
#' @export
textAreaInput <- function(inputId, label, value = "", width = NULL, height = NULL,
                          cols = NULL, rows = NULL, placeholder = NULL) {

  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      label %AND% tags$label(label, `for` = inputId),
      tags$textarea(
        id = inputId, class = "form-control", placeholder = placeholder,
        style = if (!is.null(height)) paste0("height: ", validateCssUnit(height), ";"),
        rows = rows, cols = cols, paste(value, collapse = "\n")
      )
  )
}
