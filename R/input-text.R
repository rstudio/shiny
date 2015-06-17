#' Create a text input control
#'
#' Create an input control for entry of unstructured text values
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param value Initial value.
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateTextInput}}
#'
#' @examples
#' textInput("caption", "Caption:", "Data Summary")
#' @export
textInput <- function(inputId, label, value = "") {
  div(class = "form-group shiny-input-container",
    label %AND% tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="text", class="form-control", value=value)
  )
}
