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

  attrs <- list("shinyBusySpinners" = spinners, "shinyBusyPulse" = pulse)

  js <- vapply(names(attrs), character(1), FUN = function(key) {
    if (attrs[[key]]) {
      sprintf("document.documentElement.dataset.%s = 'true';", key)
    } else {
      sprintf("delete document.documentElement.dataset.%s;", key)
    }
  })

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
#' @param ... Currently ignored (for future expansion).
#' @param spinner_type The type of spinner to use for the busy indicator. This
#'   can be any of the spinner types from the `shiny` package.
#'
#'   Available spinner types include:
#'   "`r paste0(.spinner_types, collapse = '", "')`".
#'
#'    Alternatively, you can provide a CSS URL to a custom spinner, e.g.
#'   `../spinners/my-spinning-loader.svg` (use [shiny::addResourcePath()] to
#'   ensure the `spinner` directory is accessible). Custom URLs must start with
#'   `.`, `/` or `http`.
#' @param spinner_color The color of the spinner. This can be any valid CSS
#'   color. Defaults to the app's "primary" color if Bootstrap is on the page.
#' @param spinner_size The size of the spinner. This can be any valid CSS size.
#' @param spinner_delay The amount of time to wait before showing the spinner.
#'   This can be any valid CSS time and can be useful for not showing the spinner
#'   if the computation finishes quickly.
#' @param spinner_selector A CSS selector for scoping the spinner customization.
#'   This can be useful if you want to have different spinners for different
#'   parts of the app. Defaults to the root document element. Use `NULL` to
#'   return inline styles for spinner customizations (note that you cannot
#'   customize both the pulse and the spinner without providing a selector).
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
  spinner_type = NULL,
  spinner_color = NULL,
  spinner_size = NULL,
  spinner_delay = NULL,
  spinner_selector = ":root",
  pulse_background = NULL,
  pulse_height = NULL,
  pulse_speed = NULL
) {

  rlang::check_dots_empty()

  pulse_opts <- pulseOptions(
    background = pulse_background,
    height = pulse_height,
    speed = pulse_speed
  )

  spinner_opts <- spinnerOptions(
    type = spinner_type,
    color = spinner_color,
    size = spinner_size,
    delay = spinner_delay,
    selector = spinner_selector
  )

  if (!is.null(spinner_opts) && is.null(spinner_selector)) {
    if (!is.null(pulse_opts)) {
      abort(c(
        "`spinner_selector` must be provided when customizing both the pulse and the spinner.",
        "i" = "`spinner_selector = NULL` returns inline styles for spinner customizations."
      ))
    }
    return(spinner_opts)
  }

  dropNulls(tagList(spinner_opts, pulse_opts))
}


spinnerOptions <- function(
  type = NULL,
  color = NULL,
  size = NULL,
  delay = NULL,
  selector = ":root"
) {
  if (is.null(type) && is.null(color) && is.null(size) && is.null(delay)) {
    return(NULL)
  }

  if (!is.null(type)) {
    if (!rlang::is_string(type)) {
      spinners <- paste0(.spinner_types, collapse = '", "')
      abort(
        sprintf(
          "`spinner_type` must be a string. Choose from \"%s\".",
          spinners
        )
      )
    }
    if (grepl("^(http|[./])", type)) {
      type <- sprintf("url('%s')", type)
    } else {
      type <- rlang::arg_match(type, .spinner_types)
      type <- sprintf("url('spinners/%s.svg')", type)
    }
  }

  # Options controlled via CSS variables.
  css_vars <- htmltools::css(
    `--shiny-spinner-url` = type,
    `--shiny-spinner-color` = htmltools::parseCssColors(color),
    `--shiny-spinner-size` = htmltools::validateCssUnit(size),
    `--shiny-spinner-delay` = delay
  )

  if (is.null(selector)) {
    return(css_vars)
  }

  tags$style(HTML(paste0(selector, " {", css_vars, "}")))
}

pulseOptions <- function(background = NULL, height = NULL, speed = NULL) {
  if (is.null(background) && is.null(height) && is.null(speed))  {
    return(NULL)
  }

  htmltools::css(
    `--shiny-pulse-background` = background,
    `--shiny-pulse-height` = htmltools::validateCssUnit(height),
    `--shiny-pulse-speed` = speed
  )

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
