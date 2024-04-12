#' Use and customize busy indicator types.
#'
#' To enable busy indicators, include the result of this function in the app's UI.
#'
#' When both `spinners` and `pulse` are set to `TRUE`, the pulse is disabled
#' when spinner(s) are active. When both `spinners` and `pulse` are set to
#' `FALSE`, no busy indication is shown (other than the gray-ing out of
#' recalculating outputs).
#'
#' @param spinners Overlay a spinner on each calculating/recalculating output.
#' @param pulse Show a pulsing banner at the top of the window when the server is busy.
#' @export
#' @seealso [spinnerOptions()] [pulseOptions()]
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
useBusyIndicators <- function(spinners = TRUE, pulse = TRUE) {
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


#' Customize spinning busy indicators.
#'
#' Include the result of this function in the app's UI to customize spinner
#' appearance.
#'
#' @details To effectively disable spinners, set the `size` to "0px".
#'
#' @param type The type of spinner to use. Builtin options include: tadpole,
#'   disc, dots, dot-track, and bounce. A custom type may also provided, which
#'   should be a valid value for the CSS
#'   [mask-image](https://developer.mozilla.org/en-US/docs/Web/CSS/mask-image)
#'   property.
#' @param color The color of the spinner. This can be any valid CSS color.
#'   Defaults to the app's "primary" color (if Bootstrap is on the page) or
#'   light-blue if not.
#' @param size The size of the spinner. This can be any valid CSS size.
#' @param easing The easing function to use for the spinner animation. This can
#'   be any valid CSS [easing
#'   function](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function).
#' @param speed The amount of time for the spinner to complete a single
#'   revolution. This can be any valid CSS time.
#' @param delay The amount of time to wait before showing the spinner. This can
#'   be any valid CSS time and can useful for not showing the spinner
#'   if the computation finishes quickly.
#' @param css_selector A CSS selector for scoping the spinner customization.
#'   Defaults to the root element.
#'
#' @export
#' @seealso [useBusyIndicators()] [pulseOptions()]
#' @examplesIf interactive()
#'
#' library(bslib)
#'
#' ui <- page_fillable(
#'   useBusyIndicators(),
#'   spinnerOptions(color = "orange"),
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
spinnerOptions <- function(
  type = NULL,
  ...,
  color = NULL,
  size = NULL,
  easing = NULL,
  speed = NULL,
  delay = NULL,
  css_selector = ":root"
) {

  # bounce requires a different animation than the others
  if (isTRUE(type == "bounce")) {
    animation <- "shiny-busy-spinner-bounce"
    speed <- speed %||% "0.8s"
  } else {
    animation <- NULL
  }

  # Supported types have a CSS var already defined with their SVG data
  if (isTRUE(type %in% c("tadpole", "disc", "dots", "dot-track", "bounce"))) {
    type <- sprintf("var(--_shiny-spinner-type-%s)", type)
  }

  # Options are controlled via CSS variables.
  css_vars <- paste0(c(
    if (!is.null(type)) sprintf("--shiny-spinner-mask-img: %s", type),
    if (!is.null(easing)) sprintf("--shiny-spinner-easing: %s", easing),
    if (!is.null(animation)) sprintf("--shiny-spinner-animation: %s", animation),
    if (!is.null(color)) sprintf("--shiny-spinner-color: %s", color),
    if (!is.null(size)) sprintf("--shiny-spinner-size: %s", size),
    if (!is.null(speed)) sprintf("--shiny-spinner-speed: %s", speed),
    if (!is.null(delay)) sprintf("--shiny-spinner-delay: %s", delay)
  ), collapse = ";")

  # The CSS cascade allows this to be called multiple times, and as long as the CSS
  # selector is the same, the last call takes precedence. Also, css_selector allows
  # for scoping of the spinner customization.
  tags$style(HTML(paste0(css_selector, " {", css_vars, "}")))
}


#' Customize the pulsing busy indicator.
#'
#' Include the result of this function in the app's UI to customize the pulsing
#' banner.
#'
#' @param color The color of the pulsing banner. This can be any valid CSS
#'   color. Defaults to the app's "primary" color (if Bootstrap is on the page)
#'   or light-blue if not.
#' @param height The height of the pulsing banner. This can be any valid CSS
#'   size. Defaults to "3.5px".
#' @export
#' @seealso [useBusyIndicators()] [spinnerOptions()]
pulseOptions <- function(color = NULL, height = NULL, speed = NULL) {
  css_vars <- paste0(c(
    if (!is.null(color)) sprintf("--shiny-pulse-color: %s", color),
    if (!is.null(height)) sprintf("--shiny-pulse-height: %s", height),
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
