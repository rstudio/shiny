#' Enable/disable busy indication
#'
#' To enable/disable busy indicators, include the result of this function in the
#' app's UI.
#'
#' When both `spinners` and `pulse` are set to `TRUE`, the pulse is
#' automatically disabled when spinner(s) are active. When both `spinners` and
#' `pulse` are set to `FALSE`, no busy indication is shown (other than the
#' graying out of recalculating outputs).
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
#' @examplesIf rlang::is_interactive()
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

  js <- c()

  if (rlang::is_logical(spinners) || rlang::is_string(spinners)) {
    if (rlang::is_string(spinners)) {
      spinners <- rlang::arg_match(spinners, .spinner_names)
    }
    js <- c(js, js_root_data_set_or_delete("shinyBusySpinners", spinners))
  } else {
    # TODO: better error message
    abort("Invalid value for `spinners` argument. Must be a logical or a string.")
  }

  if (rlang::is_logical(pulse)) {
    js <- c(js, js_root_data_set_or_delete("shinyBusyPulse", pulse))
  } else {
    # TODO: better error message
    abort("`pulse` must be a logical TRUE/FALSE value.")
  }

  js <- HTML(paste(js, collapse = "\n"))

  # TODO: it'd be nice if htmltools had something like a page_attrs() that allowed us
  # to do this without needing to inject JS into the head.
  tags$script(js)
}

js_root_data_set_or_delete <- function(key, value) {
  if (isTRUE(value)) {
    sprintf("document.documentElement.dataset.%s = 'true';", key)
  } else if (is.character(value) && nzchar(value)) {
    sprintf("document.documentElement.dataset.%s = '%s';", key, value)
  } else {
    sprintf("delete document.documentElement.dataset.%s;", key)
  }
}

#' Customize busy indicator options.
#'
#' To customize the appearance of the busy indicators, include the result of
#' this function in the app's UI.
#'
#' @param ... Currently ignored (for future expansion).
#' @param spinner_color The color of the spinner. This can be any valid CSS
#'   color. Defaults to the app's "primary" color if Bootstrap is on the page.
#' @param spinner_size The size of the spinner. This can be any valid CSS size.
#' @param spinner_delay The amount of time to wait before showing the spinner.
#'   This can be any valid CSS time and can be useful for not showing the spinner
#'   if the computation finishes quickly.
#' @param spinner_selector A CSS selector for scoping the spinner customization.
#'   This can be useful if you want to have different spinners for different
#'   parts of the app. Defaults to the root document element.
#' @param pulse_background A CSS background definition for the pulse. The
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
#' @examplesIf rlang::is_interactive()
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
  spinner_selector = NULL,
  pulse_background = NULL,
  pulse_height = NULL,
  pulse_speed = NULL
) {

  rlang::check_dots_empty()

  res <- tagList(
    spinnerOptions(
      color = spinner_color,
      size = spinner_size,
      delay = spinner_delay,
      selector = spinner_selector
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
spinnerOptions <- function(color = NULL, size = NULL, delay = NULL, selector = NULL) {
  if (is.null(color) && is.null(size) && is.null(delay) && is.null(selector)) {
    return(NULL)
  }

  # Options controlled via CSS variables.
  css_vars <- paste0(c(
    if (!is.null(color)) sprintf("--shiny-spinner-color: %s", htmltools::parseCssColors(color)),
    if (!is.null(size)) sprintf("--shiny-spinner-size: %s", htmltools::validateCssUnit(size)),
    if (!is.null(delay)) sprintf("--shiny-spinner-delay: %s", delay)
  ), collapse = ";")

  selector <- if (is.null(selector)) ":root" else selector

  tags$style(HTML(paste0(selector, " {", css_vars, "}")))
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
