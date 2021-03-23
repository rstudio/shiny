#' Switch Input Control
#'
#' Create a switch for toggling a logical value.
#'
#' @inheritParams checkboxInput
#' @return A switch control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [checkboxInput()], [updateSwitchInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   ui <- fluidPage(
#'     switchInput("somevalue", "Some value", FALSE),
#'     verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'     output$value <- renderText({ input$somevalue })
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' `TRUE` if checked, `FALSE` otherwise.
#'
#' @export
switchInput <- function(inputId, label, value = FALSE, width = NULL) {

  value <- restoreInput(id = inputId, default = value)

  inputTag <- tags$input(
    id = inputId, type = "checkbox",
    class = "custom-control-input",
    # TODO: checkboxInput() could do this too
    checked = if (isTRUE(value)) "checked"
  )

  labelTag <- tagAppendAttributes(
    # TODO: checkboxInput() should do this too (for accessibility)?
    shinyInputLabel(inputId, label),
    class = "custom-control-label"
  )

  div(
    class = "shiny-input-container",
    style = css(width = validateCssUnit(width)),
    div(
      class = "custom-control custom-switch",
      inputTag, labelTag
    ),
    tagFunction(function() {
      if (getCurrentVersion() < 4) {
        stop(
          "switchInput() requires Bootstrap 4 or higher. ",
          "Please supply `bslib::bs_theme()` to the UI's page layout function ",
          "(e.g., `fluidPage(theme = bslib::bs_theme())`).",
          call. = FALSE
        )
      }
    })
  )
}




#' @export
updateSwitchInput <- updateCheckboxInput
