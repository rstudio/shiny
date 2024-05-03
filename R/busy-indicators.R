#' Enable/disable busy indication
#'
#' To enable/disable busy indicators, include the result of this function in the
#' app's UI.
#'
#' When both `spinners` and `pulse` are set to `TRUE`, the pulse is
#' automatically disabled when spinner(s) are active. When both `spinners` and
#' `pulse` are set to `FALSE`, no busy indication is shown (other than the
#' gray-ing out of recalculating outputs).
#'
#' @param ... Currently ignored (for future expansion).
#' @param spinners Whether to show a spinner on each calculating/recalculating
#'   output.
#' @param pulse Whether to show a pulsing banner at the top of the page when the
#'   app is busy.
#'
#' @export
#' @seealso [busyIndicatorOptions()] for customizing the appearance of the busy
#'   indicators.
#' @examplesIf interactive()
#'
#' library(bslib)
#'
#' ui <- page_fillable(
#'   useBusyIndicators(),
#'   card(
#'     card_header(
#'       "A plot",
#'       input_task_button("simulate", "Simulate"),
#'       class = "d-flex justify-content-between align-items-center"
#'     ),
#'     plotOutput("p"),
#'   )
#' )
#'
#' server <- function(input, output) {
#'   output$p <- renderPlot({
#'     input$simulate
#'     Sys.sleep(4)
#'     plot(x = rnorm(100), y = rnorm(100))
#'   })
#' }
#'
#' shinyApp(ui, server)
useBusyIndicators <- function(..., spinners = TRUE, pulse = TRUE) {

  rlang::check_dots_empty()

  attrs <- list("shinyBusySpinners" = spinners, "shinyBusyPulse" = pulse)

  js <- Map(function(key, value) {
    if (value) {
      sprintf("document.documentElement.dataset.%s = 'true';", key)
    } else {
      sprintf("delete document.documentElement.dataset.%s;", key)
    }
  }, names(attrs), attrs)

  js <- HTML(paste(js, collapse = "\n"))

  # TODO: it'd be nice if htmltools had something like a page_attrs() that allowed us
  # to do this without needing to inject JS into the head.
  tags$script(js)
}

#' Customize busy indicator options.
#'
#' To customize the appearance of the busy indicators, include the result of
#' this function in the app's UI.
#'
#'
#' @param ... Currently ignored (for future expansion).
#' @param spinner_color The color of the spinner. This can be any valid CSS
#'   color. Defaults to the app's "primary" color (if Bootstrap is on the page).
#' @param spinner_size The size of the spinner. This can be any valid CSS size.
#' @param spinner_delay The amount of time to wait before showing the spinner.
#'   This can be any valid CSS time and can useful for not showing the spinner
#'   if the computation finishes quickly.
#' @param pulse_background A CCS background definition for the pulse. The
#'   default uses a
#'   [linear-gradient](https://developer.mozilla.org/en-US/docs/Web/CSS/gradient/linear-gradient)
#'   of the theme's indigo, purple, and pink colors.
#' @param pulse_height The height of the pulsing banner. This can be any valid
#'   CSS size.
#' @param pulse_speed The speed of the pulsing banner. This can be any valid CSS
#'   time.
#'
#' @export
#' @seealso [useBusyIndicators()] for enabling/disabling busy indicators.
#' @examplesIf interactive()
#'
#' library(bslib)
#'
#' ui <- page_fillable(
#'   useBusyIndicators(),
#'   busyIndicatorOptions(spinner_color = "orange"),
#'   card(
#'     card_header(
#'       "A plot",
#'       input_task_button("simulate", "Simulate"),
#'       class = "d-flex justify-content-between align-items-center"
#'     ),
#'     plotOutput("p"),
#'   )
#' )
#'
#' server <- function(input, output) {
#'   output$p <- renderPlot({
#'     input$simulate
#'     Sys.sleep(4)
#'     plot(x = rnorm(100), y = rnorm(100))
#'   })
#' }
#'
#' shinyApp(ui, server)
busyIndicatorOptions <- function(
  ...,
  spinner_color = NULL,
  spinner_size = NULL,
  spinner_delay = NULL,
  pulse_background = NULL,
  pulse_height = NULL,
  pulse_speed = NULL
) {

  rlang::check_dots_empty()

  res <- tagList(
    spinnerOptions(
      color = spinner_color,
      size = spinner_size,
      delay = spinner_delay
    ),
    pulseOptions(
      background = pulse_background,
      height = pulse_height,
      speed = pulse_speed
    )
  )

  dropNulls(res)
}


# TODO: allow for customization of the spinner type.
spinnerOptions <- function(color = NULL, size = NULL, delay = NULL) {
  if (is.null(color) && is.null(size) && is.null(delay))  {
    return(NULL)
  }

  # Options controlled via CSS variables.
  css_vars <- paste0(c(
    if (!is.null(color)) sprintf("--shiny-spinner-color: %s", htmltools::parseCssColors(color)),
    if (!is.null(size)) sprintf("--shiny-spinner-size: %s", htmltools::validateCssUnit(size)),
    if (!is.null(delay)) sprintf("--shiny-spinner-delay: %s", delay)
  ), collapse = ";")

  tags$style(HTML(paste0(":root {", css_vars, "}")))
}

pulseOptions <- function(background = NULL, height = NULL, speed = NULL) {
  if (is.null(background) && is.null(height) && is.null(speed))  {
    return(NULL)
  }

  css_vars <- paste0(c(
    if (!is.null(background)) sprintf("--shiny-pulse-background: %s", background),
    if (!is.null(height)) sprintf("--shiny-pulse-height: %s", htmltools::validateCssUnit(height)),
    if (!is.null(speed)) sprintf("--shiny-pulse-speed: %s", speed)
  ), collapse = ";")

  tags$style(HTML(paste0(":root {", css_vars, "}")))
}

busyIndicatorDependency <- function() {
  htmlDependency(
    name = "shiny-busy-indicators",
    version = get_package_version("shiny"),
    src = "www/shared/busy-indicators",
    package = "shiny",
    stylesheet = "busy-indicators.css",
    script = "busy-indicators.js"
  )
}
