#' Action button/link
#'
#' Creates an action button or link whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @inheritParams textInput
#' @param label The contents of the button or link--usually a text label, but
#'   you could also use any other HTML, like an image.
#' @param icon An optional [icon()] to appear on the button.
#' @param disabled If `TRUE`, the button will not be clickable. Use
#'   [updateActionButton()] to dynamically enable/disable the button.
#' @param ... Named attributes to be applied to the button or link.
#'
#' @family input elements
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sliderInput("obs", "Number of observations", 0, 1000, 500),
#'   actionButton("goButton", "Go!", class = "btn-success"),
#'   plotOutput("distPlot")
#' )
#'
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     # Take a dependency on input$goButton. This will run once initially,
#'     # because the value changes from NULL to 0.
#'     input$goButton
#'
#'     # Use isolate() to avoid dependency on input$obs
#'     dist <- isolate(rnorm(input$obs))
#'     hist(dist)
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#' ## Example of adding extra class values
#' actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-lg")
#' actionLink("infoLink", "Information Link", class = "btn-info")
#'
#' @seealso [observeEvent()] and [eventReactive()]
#'
#' @section Server value:
#' An integer of class `"shinyActionButtonValue"`. This class differs from
#' ordinary integers in that a value of 0 is considered "falsy".
#' This implies two things:
#'   * Event handlers (e.g., [observeEvent()], [eventReactive()]) won't execute on initial load.
#'   * Input validation (e.g., [req()], [need()]) will fail on initial load.
#' @export
actionButton <- function(inputId, label, icon = NULL, width = NULL,
  disabled = FALSE, ...) {

  value <- restoreInput(id = inputId, default = NULL)

  tags$button(id=inputId,
    style = css(width = validateCssUnit(width)),
    type="button",
    class="btn btn-default action-button",
    `data-val` = value,
    disabled = if (isTRUE(disabled)) NA else NULL,
    list(validateIcon(icon), label),
    ...
  )
}

#' @rdname actionButton
#' @export
actionLink <- function(inputId, label, icon = NULL, ...) {
  value <- restoreInput(id = inputId, default = NULL)

  tags$a(id=inputId,
    href="#",
    class="action-button",
    `data-val` = value,
    list(validateIcon(icon), label),
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
validateIcon <- function(icon) {
  if (is.null(icon) || identical(icon, character(0))) {
    return(icon)
  } else if (inherits(icon, "shiny.tag") && icon$name == "i") {
    return(icon)
  } else {
    stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
  }
}
