#' Create a password input control
#'
#' Create an password control for entry of passwords.
#'
#' @inheritParams textInput
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateTextInput()]
#'
#' @section Server value:
#' A character string of the password input. The default value is `""`
#' unless `value` is provided.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   passwordInput("password", "Password:"),
#'   actionButton("go", "Go"),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({
#'     req(input$go)
#'     isolate(input$password)
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
passwordInput <- function(inputId, label, value = "", width = NULL,
                          placeholder = NULL, ..., updateOn = c("change", "blur")) {
  rlang::check_dots_empty()
  updateOn <- rlang::arg_match(updateOn)

  div(class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),
    tags$input(id = inputId, type="password", class="shiny-input-password form-control", value=value,
               placeholder = placeholder, `data-update-on` = updateOn)
  )
}
