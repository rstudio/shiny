#' Slider Input Widget
#'
#' Constructs a slider widget to select a numeric value from a range.
#'
#' @inheritParams textInput
#' @param min The minimum value (inclusive) that can be selected.
#' @param max The maximum value (inclusive) that can be selected.
#' @param value The initial value of the slider. A numeric vector of length one
#'   will create a regular slider; a numeric vector of length two will create a
#'   double-ended range slider. A warning will be issued if the value doesn't
#'   fit between `min` and `max`.
#' @param step Specifies the interval between each selectable value on the
#'   slider (if `NULL`, a heuristic is used to determine the step size). If
#'   the values are dates, `step` is in days; if the values are times
#'   (POSIXt), `step` is in seconds.
#' @param round `TRUE` to round all values to the nearest integer;
#'   `FALSE` if no rounding is desired; or an integer to round to that
#'   number of digits (for example, 1 will round to the nearest 10, and -2 will
#'   round to the nearest .01). Any rounding will be applied after snapping to
#'   the nearest step.
#' @param format Deprecated.
#' @param locale Deprecated.
#' @param ticks `FALSE` to hide tick marks, `TRUE` to show them
#'   according to some simple heuristics.
#' @param animate `TRUE` to show simple animation controls with default
#'   settings; `FALSE` not to; or a custom settings list, such as those
#'   created using [animationOptions()].
#' @param sep Separator between thousands places in numbers.
#' @param pre A prefix string to put in front of the value.
#' @param post A suffix string to put after the value.
#' @param dragRange This option is used only if it is a range slider (with two
#'   values). If `TRUE` (the default), the range can be dragged. In other
#'   words, the min and max can be dragged together. If `FALSE`, the range
#'   cannot be dragged.
#' @param timeFormat Only used if the values are Date or POSIXt objects. A time
#'   format string, to be passed to the Javascript strftime library. See
#'   <https://github.com/samsonjs/strftime> for more details. The allowed
#'   format specifications are very similar, but not identical, to those for R's
#'   [base::strftime()] function. For Dates, the default is `"%F"`
#'   (like `"2015-07-01"`), and for POSIXt, the default is `"%F %T"`
#'   (like `"2015-07-01 15:32:10"`).
#' @param timezone Only used if the values are POSIXt objects. A string
#'   specifying the time zone offset for the displayed times, in the format
#'   `"+HHMM"` or `"-HHMM"`. If `NULL` (the default), times will
#'   be displayed in the browser's time zone. The value `"+0000"` will
#'   result in UTC time.
#' @param skin controls the overall look of the slider. For a demo of different
#' skins, see <http://ionden.com/a/plugins/ion.rangeSlider/skins.html>.
#' @param accentColor a CSS color string (e.g., `'#ED5564'`) or Bootstrap Sass
#' variable (e.g., `'$primary'`) to use for the `style`'s accent color. If a Sass
#' variable is used, it must match a Sass variable name of a currently active
#' **bootstraplib** theme.
#' @param sassVariables a list of Sass variables to control particular CSS rules in a
#' given `skin`. To see what variables are available, see the relevant `style` scss
#' file(s) [here](https://github.com/rstudio/shiny/blob/master/inst/www/shared/ionrangeslider/scss/skins)
#' @inheritParams selectizeInput
#' @family input elements
#' @seealso [updateSliderInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   sliderInput("obs", "Number of observations:",
#'     min = 0, max = 1000, value = 500
#'   ),
#'   plotOutput("distPlot")
#' )
#'
#' # Server logic
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     hist(rnorm(input$obs))
#'   })
#' }
#'
#' # Complete app with UI and server components
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#' A number, or in the case of slider range, a vector of two numbers.
#'
#' @export
sliderInput <- function(inputId, label, min, max, value, step = NULL,
                        round = FALSE, format = NULL, locale = NULL,
                        ticks = TRUE, animate = FALSE, width = NULL, sep = ",",
                        pre = NULL, post = NULL, timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE,
                        skin = c("shiny", "big", "flat", "modern", "round", "sharp", "square"),
                        accentColor = NULL, sassVariables = NULL) {
  if (!missing(format)) {
    shinyDeprecated(msg = "The `format` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.",
                    version = "0.10.2.2")
  }
  if (!missing(locale)) {
    shinyDeprecated(msg = "The `locale` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.",
                    version = "0.10.2.2")
  }

  dataType <- getSliderType(min, max, value)

  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }

  # Restore bookmarked values here, after doing the type checking, because the
  # restored value will be a character vector instead of Date or POSIXct, and we can do
  # the conversion to correct type next.
  value <- restoreInput(id = inputId, default = value)

  if (is.character(value)) {
    # If we got here, the value was restored from a URL-encoded bookmark.
    if (dataType == "date") {
      value <- as.Date(value, format = "%Y-%m-%d")
    } else if (dataType == "datetime") {
      # Date-times will have a format like "2018-02-28T03:46:26Z"
      value <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }

  step <- findStepSize(min, max, step)

  if (dataType %in% c("date", "datetime")) {
    # For Dates, this conversion uses midnight on that date in UTC
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))

    # Convert values to milliseconds since epoch (this is the value JS uses)
    # Find step size in ms
    step  <- to_ms(max) - to_ms(max - step)
    min   <- to_ms(min)
    max   <- to_ms(max)
    value <- to_ms(value)
  }

  range <- max - min

  # Try to get a sane number of tick marks
  if (ticks) {
    n_steps <- range / step

    # Make sure there are <= 10 steps.
    # n_ticks can be a noninteger, which is good when the range is not an
    # integer multiple of the step size, e.g., min=1, max=10, step=4
    scale_factor <- ceiling(n_steps / 10)
    n_ticks <- n_steps / scale_factor

  } else {
    n_ticks <- NULL
  }

  skin <- match.arg(skin)

  sliderProps <- dropNulls(list(
    class = "js-range-slider",
    id = inputId,
    `data-skin` = skin,
    `data-type` = if (length(value) > 1) "double",
    `data-min` = formatNoSci(min),
    `data-max` = formatNoSci(max),
    `data-from` = formatNoSci(value[1]),
    `data-to` = if (length(value) > 1) formatNoSci(value[2]),
    `data-step` = formatNoSci(step),
    `data-grid` = ticks,
    `data-grid-num` = n_ticks,
    `data-grid-snap` = FALSE,
    `data-prettify-separator` = sep,
    `data-prettify-enabled` = (sep != ""),
    `data-prefix` = pre,
    `data-postfix` = post,
    `data-keyboard` = TRUE,
    # This value is only relevant for range sliders; for non-range sliders it
    # causes problems since ion.RangeSlider 2.1.2 (issue #1605).
    `data-drag-interval` = if (length(value) > 1) dragRange,
    # The following are ignored by the ion.rangeSlider, but are used by Shiny.
    `data-data-type` = dataType,
    `data-time-format` = timeFormat,
    `data-timezone` = timezone
  ))

  # Replace any TRUE and FALSE with "true" and "false"
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) "true"
    else if (identical(x, FALSE)) "false"
    else x
  })

  sliderTag <- div(class = "form-group shiny-input-container",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    shinyInputLabel(inputId, label),
    do.call(tags$input, sliderProps)
  )

  # Add animation buttons
  if (identical(animate, TRUE))
    animate <- animationOptions()

  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- icon('play', lib = 'glyphicon')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- icon('pause', lib = 'glyphicon')

    sliderTag <- tagAppendChild(
      sliderTag,
      tags$div(class='slider-animate-container',
        tags$a(href='#',
          class='slider-animate-button',
          'data-target-id'=inputId,
          'data-interval'=animate$interval,
          'data-loop'=animate$loop,
          span(class = 'play', animate$playButton),
          span(class = 'pause', animate$pauseButton)
        )
      )
    )
  }

  # Any of these cases require Sass compilation
  if (useBsTheme() || length(accentColor) || length(sassVariables)) {
    scssDir <- system.file(package = "shiny", "www", "shared", "ionrangeslider", "scss")
    tmpDir <- tempfile("ion-scss")
    dir.create(tmpDir)
    outFile <- file.path(tmpDir, "slider-custom.css")
    sassFunc <- if (useBsTheme()) bootstraplib::bootstrap_sass else sass::sass
    defaults <- list(
      sassVariables %OR% "",
      accentVariableDefaults(skin, accentColor),
      bsThemeSliderDefaults(skin)
    )
    sassFunc(
      list(
        defaults,
        sass::sass_file(file.path(scssDir, "_base.scss")),
        sass::sass_file(file.path(scssDir, "skins", paste0(skin, ".scss")))
      ),
      output = outFile
    )
    # Hash the skin and defaults so that the html dependency from one slider won't stomp another
    name <- paste0("ionrangeslider-", digest::digest(list(skin, defaults), "xxhash64"))
    cssDependency <- htmlDependency(
      name, "2.3.1",
      src = dirname(outFile),
      stylesheet = basename(outFile)
    )
  } else {
    cssDependency <- htmlDependency(
      paste0("ionrangeslider-", skin), "2.3.1",
      src = c(href = "shared/ionrangeslider"),
      # ion.rangeSlider also needs normalize.css, which is already included in
      # Bootstrap.
      stylesheet = c(
        "css/ion.rangeSlider.css",
        sprintf("css/ion.rangeSlider.skin%s.css", tools::toTitleCase(skin))
      )
    )
  }

  attachDependencies(
    sliderTag, list(
      cssDependency,
      # Include ion JS as a separate dependency so that we aren't duplicating it
      htmlDependency(
        "ionrangeslider-javascript", "2.3.1",
        src = c(href = "shared/ionrangeslider"),
        script = "js/ion.rangeSlider.min.js"
      ),
      htmlDependency(
        "strftime", "0.9.2",
        src = c(href = "shared/strftime"),
        script = "strftime-min.js"
      )
    )
  )
}

hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return (!identical(value, truncatedValue))
}


accentVariableDefaults <- function(skin, accentColor = NULL) {
  if (useBsTheme()) {
    primary <- if ("3" %in% bootstraplib::theme_version()) "$brand-primary" else "$primary"
    accentColor <- parseAccentColor(accentColor %OR% primary)
    accentColorDark <- paste0("mix(", accentColor, ", $black, 80%)")
    accentColorLight <- paste0("mix(", accentColor, ", $white, 10%)")
  } else {
    accentColor <- parseAccentColor(accentColor %OR% defaultAccentColor(skin))
    accentColorDark <- paste0("mix(", accentColor, ", black, 80%)")
    accentColorLight <- paste0("mix(", accentColor, ", white, 10%)")
  }
  vars <- switch(skin,
    big = list(
      "bar_color"     = accentColor,
      "label_color_1" = accentColor,
      "grid_color_1"  = accentColor
    ),
    flat = list(
      "bar_color"      = accentColor,
      "handle_color_1" = accentColorDark,
      "handle_color_2" = accentColorDark,
      "label_color_1"  = accentColor
    ),
    modern = list(
      "bar_color"     = accentColor,
      "label_color_1" = accentColor
    ),
    round = list(
      "bar_color"      = accentColor,
      "handle_color_1" = accentColor,
      "handle_color_3" = accentColorLight,
      "label_color_1"  = accentColor
    ),
    sharp = list(
      "bar_color"       = accentColor,
      "handle_color_1"  = accentColorDark,
      "minmax_bg_color" = accentColorDark,
      "label_color_1"   = accentColorDark
    ),
    shiny = list("accent" = accentColor),
    square = list(
      "bar_color"      = accentColor,
      "handle_color_1" = accentColor,
      "handle_color_3" = accentColorLight,
      "label_color_1"  = accentColor
    ),
    stop("Couldn't find skin named '", skin, "'", call. = FALSE)
  )

  lapply(vars, function(x) paste(x, "!default"))
}

# TODO: BS3 version!
bsThemeSliderDefaults <- function(skin) {
  if (!useBsTheme()) {
    return("")
  }

  vars <- switch(skin,
    big = list(
      "line_color_1"      = "$white",
      "line_color_2"      = "gray('200')",
      "line_color_3"      = "gray('100')",
      "handle_color_1"    = "gray('200')",
      "handle_color_2"    = "gray('300')",
      "handle_color_3"    = "$white",
      "handle_color_4"    = "gray('400')",
      "minmax_text_color" = "$white",
      "minmax_bg_color"   = "gray('500')",
      "label_color_2"     = "$white"
    ),
    flat = list(
      "line_color"        = "gray('200')",
      "minmax_text_color" = "gray('600')",
      "minmax_bg_color"   = "gray('200')",
      "label_color_2"     = "$white",
      "grid_color_1"      = "gray('200')",
      "grid_color_2"      = "gray('600')"
    ),
    modern = list(
      "line_color"        = "gray('200')",
      "handle_color_1"    = "gray('100')",
      "handle_color_2"    = "$white",
      "handle_color_3"    = "gray('500')",
      "handle_color_4"    = "gray('700')",
      "minmax_text_color" = "$white",
      "minmax_bg_color"   = "gray('400')",
      "label_color_2"     = "$white",
      "grid_color_1"      = "gray('300')",
      "grid_color_2"      = "gray('500')"
    ),
    round = list(
      "line_color"        = "gray('200')",
      "handle_color_2"    = "$white",
      "minmax_text_color" = "gray('800')",
      "minmax_bg_color"   = "gray('300')",
      "label_color_2"     = "$white",
      "grid_color_1"      = "gray('300')",
      "grid_color_2"      = "gray('500')"
    ),
    sharp = list(
      "line_color"        = "$black",
      "handle_color_2"    = "$white",
      "handle_color_3"    = "$black",
      "minmax_text_color" = "$white",
      "label_color_2"     = "$white",
      "grid_color_1"      = "gray('200')",
      "grid_color_2"      = "gray('400')"
    ),
    shiny = list(
      "bg" = "$white",
      "fg" = "$black"
    ),
    square = list(
      "line_color"        = "gray('200')",
      "handle_color_2"    = "$white",
      "minmax_text_color" = "gray('800')",
      "minmax_bg_color"   = "gray('200')",
      "label_color_2"     = "$white",
      "grid_color_1"      = "gray('200')",
      "grid_color_2"      = "gray('400')"
    ),
    stop("Couldn't find skin named '", skin, "'", call. = FALSE)
  )

  lapply(vars, function(x) paste(x, "!default"))
}

defaultAccentColor <- function(skin) {
  switch(
    skin, big = "#428bca", flat = "#ed5565", modern = "#20b426",
    round = "#006cfa", sharp = "#ee22fa", shiny = "#428bca", square = "black",
    stop("Couldn't find skin named '", skin, "'", call. = FALSE)
  )
}

parseAccentColor <- function(x) {
  if (length(x) != 1) {
    stop("`accentColor` must be of length 1", call. = FALSE)
  }
  color <- parseCssColors(x, mustWork = FALSE)
  if (!is.na(color)) {
    return(color)
  }
  if (useBsTheme()) {
    color <- parseCssColors(
      bootstraplib::bs_theme_get_variables(x),
      mustWork = FALSE
    )
    if (!is.na(color)) {
      return(color)
    }
    stop("Couldn't find a Bootstrap Sass variable named '", x, "' with a CSS color string value.", call. = FALSE)
  }
  stop("Failed to parse the accent color '", x, "' as a CSS color.", call. = FALSE)
}


# If step is NULL, use heuristic to set the step size.
findStepSize <- function(min, max, step) {
  if (!is.null(step)) return(step)

  range <- max - min
  # If short range or decimals, use continuous decimal with ~100 points
  if (range < 2 || hasDecimals(min) || hasDecimals(max)) {
    # Workaround for rounding errors (#1006): the intervals between the items
    # returned by pretty() can have rounding errors. To avoid this, we'll use
    # pretty() to find the min, max, and number of steps, and then use those
    # values to calculate the step size.
    pretty_steps <- pretty(c(min, max), n = 100)
    n_steps <- length(pretty_steps) - 1

    # Fix for #2061: Windows has low-significance digits (like 17 digits out)
    # even at the boundaries of pretty()'s output. Use signif(digits = 10),
    # which should be way way less significant than any data we'd want to keep.
    # It might make sense to use signif(steps[2] - steps[1], 10) instead, but
    # for now trying to make the minimal change.
    signif(digits = 10, (max(pretty_steps) - min(pretty_steps)) / n_steps)

  } else {
    1
  }
}


#' @rdname sliderInput
#'
#' @param interval The interval, in milliseconds, between each animation step.
#' @param loop `TRUE` to automatically restart the animation when it
#'   reaches the end.
#' @param playButton Specifies the appearance of the play button. Valid values
#'   are a one-element character vector (for a simple text label), an HTML tag
#'   or list of tags (using [tag()] and friends), or raw HTML (using
#'   [HTML()]).
#' @param pauseButton Similar to `playButton`, but for the pause button.
#' @export
animationOptions <- function(interval=1000,
                             loop=FALSE,
                             playButton=NULL,
                             pauseButton=NULL) {
  list(interval=interval,
       loop=loop,
       playButton=playButton,
       pauseButton=pauseButton)
}
