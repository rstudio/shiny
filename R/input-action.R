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

  icon <- validateIcon(icon)

  if (!is.null(icon)) {
    icon <- span(icon, class = "action-icon", .noWS = c("outside", "inside"))
  }

  if (!is.null(label)) {
    label <- span(label, class = "action-label", .noWS = c("outside", "inside"))
  }

  tags$button(
    id = inputId,
    style = css(width = validateCssUnit(width)),
    type = "button",
    class = "btn btn-default action-button",
    `data-val` = value,
    disabled = if (isTRUE(disabled)) NA else NULL,
    icon, label,
    .noWS = "inside",
    ...
  )
}

#' @rdname actionButton
#' @export
actionLink <- function(inputId, label, icon = NULL, ...) {
  value <- restoreInput(id = inputId, default = NULL)

  icon <- validateIcon(icon)

  if (!is.null(icon)) {
    icon <- span(icon, class = "action-icon", .noWS = c("outside", "inside"))
  }

  if (!is.null(label)) {
    label <- span(label, class = "action-label", .noWS = c("outside", "inside"))
  }

  tags$a(
    id = inputId,
    href = "#",
    class = "action-button action-link",
    `data-val` = value,
    icon, label,
    .noWS = "inside",
    ...
  )
}


# Throw an informative warning if icon isn't html-ish
validateIcon <- function(icon) {
  if (length(icon) == 0) {
    return(icon)
  }
  if (!isTagLike(icon)) {
    rlang::warn(
      c(
        "It appears that a non-HTML value was provided to `icon`.",
        i = "Try using a `shiny::icon()` (or an equivalent) to get an icon."
      ),
      class = "shiny-validate-icon"
    )
  }
  icon
}
