#' Action button/link
#'
#' Creates an action button or link whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @inheritParams textInput
#' @param label The contents of the button or link--usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param icon An optional \code{\link{icon}} to appear on the button.
#' @param ... Named attributes to be applied to the button or link.
#'
#' @family input elements
#' @examples
#' \dontrun{
#' # In server.R
#' output$distPlot <- renderPlot({
#'   # Take a dependency on input$goButton
#'   input$goButton
#'
#'   # Use isolate() to avoid dependency on input$obs
#'   dist <- isolate(rnorm(input$obs))
#'   hist(dist)
#' })
#'
#' # In ui.R
#' actionButton("goButton", "Go!")
#' }
#'
#' @seealso \code{\link{observeEvent}} and \code{\link{eventReactive}}
#'
#' @export
actionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  tags$button(id=inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    type="button",
    class="btn btn-default action-button",
    list(icon, label),
    ...
  )
}

#' @rdname actionButton
#' @export
actionLink <- function(inputId, label, icon = NULL, ...) {
  tags$a(id=inputId,
    href="#",
    class="action-button",
    list(icon, label),
    ...
  )
}
