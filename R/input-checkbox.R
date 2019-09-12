#' Checkbox Input Control
#'
#' Create a checkbox that can be used to specify logical values.
#'
#' @inheritParams textInput
#' @param value Initial value (`TRUE` or `FALSE`).
#' @return A checkbox control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [checkboxGroupInput()], [updateCheckboxInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   checkboxInput("somevalue", "Some value", FALSE),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$somevalue })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' `TRUE` if checked, `FALSE` otherwise.
#'
#' @export
checkboxInput <- function(inputId, label, value = FALSE, width = NULL) {

  value <- restoreInput(id = inputId, default = value)

  inputTag <- tags$input(id = inputId, type="checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"

  div(class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    div(class = "checkbox",
      tags$label(inputTag, tags$span(label))
    )
  )
}
