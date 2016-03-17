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
  # currently not using this return value to ensure backwards compatibility
  validIcon(icon)
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
  # currently not using this return value to ensure backwards compatibility
  validIcon(icon)
  tags$a(id=inputId,
    href="#",
    class="action-button",
    list(icon, label),
    ...
  )
}


# Check that the icon parameter is valid:
# 1) Check  if the user wants to actually add an icon:
#    -- if icon=NULL, it means leave the icon unchanged
#    -- if icon=character(0), it means don't add an icon or, more usefully,
#       remove the previous icon
# 2) If so, check that the icon has the right format (this does not check whether
# it is a *real* icon - currently that would require a massive cross reference
# with the "font-awesome" and the "glyphicon" libraries)
validIcon <- function(icon) {
  if (is.null(icon) || identical(icon, character(0))) return(TRUE)
  else if (inherits(icon, "shiny.tag")) if (icon$name == "i") return(TRUE)
  else {
    warning("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
    return(FALSE)
  }
}
