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
    checked = if (isTRUE(value)) "checked"
  )

  # TODO: checkboxInput() should do this too (for accessibility)?
  labelTag <- shinyInputLabel(inputId, label)

  tagFunction(function() {
    if (getCurrentVersion() < 4) {
      stop(
        "switchInput() requires Bootstrap 4 or higher. ",
        "Please supply `bslib::bs_theme()` to the UI's page layout function ",
        "(e.g., `fluidPage(theme = bslib::bs_theme())`).",
        call. = FALSE
      )
    }

    isBS4 <- getCurrentVersion() == 4
    div(
      class = "shiny-input-container",
      style = css(width = validateCssUnit(width)),
      div(
        class = if (isBS4) "custom-control custom-switch" else "form-check form-switch",
        tagAppendAttributes(
          inputTag, class = if (isBS4) "custom-control-input" else "form-check-input"
        ),
        tagAppendAttributes(
          labelTag, class = if (isBS4) "custom-control-label" else "form-check-label"
        )
      )
    )
  })
}
