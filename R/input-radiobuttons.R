#' Create radio buttons
#'
#' Create a set of radio buttons used to select an item from a list.
#'
#' If you need to represent a "None selected" state, it's possible to default
#' the radio buttons to have no options selected by using
#' \code{selected = character(0)}. However, this is not recommended, as it gives
#' the user no way to return to that state once they've made a selection.
#' Instead, consider having the first of your choices be \code{c("None selected"
#' = "")}.
#'
#' @inheritParams textInput
#' @param choices List of values to select from (if elements of the list are
#' named then that name rather than the value is displayed to the user)
#' @param selected The initially selected value (if not specified then
#' defaults to the first value)
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally)
#' @return A set of radio buttons that can be added to a UI definition.
#'
#' @family input elements
#' @seealso \code{\link{updateRadioButtons}}
#'
#' @examples
#' radioButtons("dist", "Distribution type:",
#'              c("Normal" = "norm",
#'                "Uniform" = "unif",
#'                "Log-normal" = "lnorm",
#'                "Exponential" = "exp"))
#' @export
radioButtons <- function(inputId, label, choices, selected = NULL,
  inline = FALSE, width = NULL) {

  # resolve names
  choices <- choicesWithNames(choices)

  # default value if it's not specified
  selected <- if (is.null(selected)) choices[[1]] else {
    validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")

  options <- generateOptions(inputId, choices, selected, inline, type = 'radio')

  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline)
    divClass <- paste(divClass, "shiny-input-container-inline")

  tags$div(id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    class = divClass,
    controlLabel(inputId, label),
    options
  )
}
