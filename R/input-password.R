#' Create a password input control
#'
#' Create an password control for entry of passwords.
#'
#' @inheritParams textInput
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateTextInput}}
#'
#' @examples
#' passwordInput("password", "Password:")
#' @export
passwordInput <- function(inputId, label, value = "") {
  div(class = "form-group shiny-input-container",
    label %AND% tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="password", class="form-control", value=value)
  )
}
