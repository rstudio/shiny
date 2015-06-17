#' Create a submit button
#'
#' Create a submit button for an input form. Forms that include a submit
#' button do not automatically update their outputs when inputs change,
#' rather they wait until the user explicitly clicks the submit button.
#'
#' @param text Button caption
#' @param icon Optional \code{\link{icon}} to appear on the button
#' @return A submit button that can be added to a UI definition.
#'
#' @family input elements
#'
#' @examples
#' submitButton("Update View")
#' submitButton("Update View", icon("refresh"))
#' @export
submitButton <- function(text = "Apply Changes", icon = NULL) {
  div(
    tags$button(type="submit", class="btn btn-primary", list(icon, text))
  )
}
