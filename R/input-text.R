#' Create a text input control
#'
#' Create an input control for entry of unstructured text values
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param value Initial value.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
#'   see \code{\link{validateCssUnit}}.
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
#' @return A text input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateTextInput}}
#'
#' @examples
#' textInput("caption", "Caption:", "Data Summary")
#' @export
textInput <- function(inputId, label, value = "", width = NULL,
  placeholder = NULL) {

  div(class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    label %AND% tags$label(label, `for` = inputId),
    tags$input(id = inputId, type="text", class="form-control", value=value,
      placeholder = placeholder)
  )
}
