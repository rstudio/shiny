#' Checkbox Group Input Control
#'
#' Create a group of checkboxes that can be used to toggle multiple choices
#' independently. The server will receive the input as a character vector of the
#' selected values.
#'
#' @inheritParams textInput
#' @param choices List of values to show checkboxes for. If elements of the list
#'   are named then that name rather than the value is displayed to the user.
#' @param selected The values that should be initially selected, if any.
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally)
#' @return A list of HTML elements that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{checkboxInput}}, \code{\link{updateCheckboxGroupInput}}
#'
#' @examples
#' checkboxGroupInput("variable", "Variable:",
#'                    c("Cylinders" = "cyl",
#'                      "Transmission" = "am",
#'                      "Gears" = "gear"))
#'
#' @export
checkboxGroupInput <- function(inputId, label, choices, selected = NULL,
  inline = FALSE, width = NULL) {

  # resolve names
  choices <- choicesWithNames(choices)
  if (!is.null(selected))
    selected <- validateSelected(selected, choices, inputId)

  options <- generateOptions(inputId, choices, selected, inline)

  divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
  if (inline)
    divClass <- paste(divClass, "shiny-input-container-inline")

  # return label and select tag
  tags$div(id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    class = divClass,
    controlLabel(inputId, label),
    options
  )
}
