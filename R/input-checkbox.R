#' Checkbox Input Control
#'
#' Create a checkbox that can be used to specify logical values.
#'
#' @inheritParams textInput
#' @param value Initial value (\code{TRUE} or \code{FALSE}).
#' @return A checkbox control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{checkboxGroupInput}}, \code{\link{updateCheckboxInput}}
#'
#' @examples
#' checkboxInput("outliers", "Show outliers", FALSE)
#' @export
checkboxInput <- function(inputId, label, value = FALSE) {
  inputTag <- tags$input(id = inputId, type="checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"

  div(class = "form-group shiny-input-container",
    div(class = "checkbox",
      tags$label(inputTag, tags$span(label))
    )
  )
}
