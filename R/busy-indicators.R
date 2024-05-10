#' Enable/disable busy indication
#'
#' Busy indicators provide a visual cue to users when the server is busy
#' calculating outputs or otherwise performing tasks (e.g., producing
#' downloads). When enabled, a spinner is shown on each
#' calculating/recalculating output, and a pulsing banner is shown at the top of
#' the page when the app is otherwise busy. Busy indication is enabled by
#' default for UI created with \pkg{bslib}, but must be enabled otherwise. To
#' enable/disable, include the result of this function in anywhere in the app's
#' UI.
#'
#' When both `spinners` and `pulse` are set to `TRUE`, the pulse is
#' automatically disabled when spinner(s) are active. When both `spinners` and
#' `pulse` are set to `FALSE`, no busy indication is shown (other than the
#' graying out of recalculating outputs).
#'
#' @param ... Currently ignored.
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

#' Customize busy indicator options
#'
#' When busy indicators are enabled (see [useBusyIndicators()]), a spinner is
#' shown on each calculating/recalculating output, and a pulsing banner is shown
#' at the top of the page when the app is otherwise busy. This function allows
#' you to customize the appearance of those busy indicators. To apply the
#' customization, include the result of this function inside the app's UI.
#'
#' @param ... Currently ignored.
#' @param spinner_type The type of spinner. Pre-bundled types include:
#'   '`r paste0(.busySpinnerTypes, collapse = "', '")`'.
#'
#'   A path to a local SVG file can also be provided. The SVG should adhere to
#'   the following rules:
#'   * The SVG itself should contain the animation.
#'   * It should avoid absolute sizes (the spinner's containing DOM element
#'     size is set in CSS by `spinner_size`, so it should fill that container).
#'   * It should avoid setting absolute colors (the spinner's containing DOM element
#'     color is set in CSS by `spinner_color`, so it should inherit that color).
#' @param spinner_color The color of the spinner. This can be any valid CSS
#'   color. Defaults to the app's "primary" color if Bootstrap is on the page.
#' @param spinner_size The size of the spinner. This can be any valid CSS size.
#' @param spinner_delay The amount of time to wait before showing the spinner.
#'   This can be any valid CSS time and can be useful for not showing the spinner
#'   if the computation finishes quickly.
#' @param spinner_selector A character string containing a CSS selector for
#'   scoping the spinner customization. The default (`NULL`) applies the
#'   customization globally (i.e., to the root DOM element). Can also be set to
#'   `NA` to apply the customization as inline styles to a specific element
#'   (e.g., `div(busyIndicatorOptions(..., spinner_selector = NA))`).
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
#' card_ui <- function(id, spinner_type = id) {
#'   card(
#'     busyIndicatorOptions(
#'       spinner_type = spinner_type,
#'       spinner_selector = NA
#'     ),
#'     card_header(paste("Spinner:", spinner_type)),
#'     plotOutput(shiny::NS(id, "plot"))
#'   )
#' }
#'
#' card_server <- function(id, simulate = reactive()) {
#'   moduleServer(
#'     id = id,
#'     function(input, output, session) {
#'       output$plot <- renderPlot({
#'         Sys.sleep(1)
#'         simulate()
#'         plot(x = rnorm(100), y = rnorm(100))
#'       })
#'     }
#'   )
#' }
#'
#' ui <- page_fillable(
#'   useBusyIndicators(),
#'   input_task_button("simulate", "Simulate", icon = icon("refresh")),
#'   layout_columns(
#'     card_ui("ring"),
#'     card_ui("bars"),
#'     card_ui("dots"),
#'     card_ui("pulse"),
#'     col_widths = 6
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   simulate <- reactive(input$simulate)
#'   card_server("ring", simulate)
#'   card_server("bars", simulate)
#'   card_server("dots", simulate)
#'   card_server("pulse", simulate)
#' }
#'
#' shinyApp(ui, server)
#'
busyIndicatorOptions <- function(
  ...,
  spinner_type = NULL,
  spinner_color = NULL,
  spinner_size = NULL,
  spinner_delay = NULL,
  spinner_selector = NULL,
  pulse_background = NULL,
  pulse_height = NULL,
  pulse_speed = NULL
) {

  rlang::check_dots_empty()

  spinner <- spinnerOptions(
    type = spinner_type,
    color = spinner_color,
    size = spinner_size,
    delay = spinner_delay,
    selector = spinner_selector
  )

  pulse <- pulseOptions(
    background = pulse_background,
    height = pulse_height,
    speed = pulse_speed
  )

  if (isTRUE(is.na(spinner_selector))) {
    if (!is.null(pulse)) {
      abort("pulse cannot be customized when spinner_selector is set to NA")
    }
    return(spinner)
  }

  dropNulls(tagList(spinner, pulse))
}


spinnerOptions <- function(type = NULL, color = NULL, size = NULL, delay = NULL, selector = NULL) {
  if (is.null(type) && is.null(color) && is.null(size) && is.null(delay) && is.null(selector)) {
    return(NULL)
  }

  url <- NULL
  if (!is.null(type)) {
    stopifnot(is.character(type) && length(type) == 1)
    if (file.exists(type) && grepl("\\.svg$", type)) {
      typeRaw <- readBin(type, "raw", n = file.info(type)$size)
      url <- sprintf("url('data:image/svg+xml;base64,%s')", rawToBase64(typeRaw))
    } else {
      type <- rlang::arg_match(type, .busySpinnerTypes)
      url <- sprintf("url('spinners/%s.svg')", type)
    }
  }

  # Options controlled via CSS variables.
  css_vars <- htmltools::css(
    "--shiny-spinner-url" = url,
    "--shiny-spinner-color" = htmltools::parseCssColors(color),
    "--shiny-spinner-size" = htmltools::validateCssUnit(size),
    "--shiny-spinner-delay" = delay
  )

  if (isTRUE(is.na(selector))) {
    return(rlang::splice(list(style = css_vars)))
  }

  selector <- if (is.null(selector)) ":root" else selector

  tags$style(HTML(paste0(selector, " {", css_vars, "}")))
}

pulseOptions <- function(background = NULL, height = NULL, speed = NULL) {
  if (is.null(background) && is.null(height) && is.null(speed))  {
    return(NULL)
  }

  css_vars <- htmltools::css(
    "--shiny-pulse-background" = background,
    "--shiny-pulse-height" = htmltools::validateCssUnit(height),
    "--shiny-pulse-speed" = speed
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
