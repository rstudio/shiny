#' Slider Input Widget
#'
#' Constructs a slider widget to select a numeric value from a range.
#'
#' @param inputId Specifies the \code{input} slot that will be used to access
#'   the value.
#' @param label A descriptive label to be displayed with the widget, or
#'   \code{NULL}.
#' @param min The minimum value (inclusive) that can be selected.
#' @param max The maximum value (inclusive) that can be selected.
#' @param value The initial value of the slider. A numeric vector of length
#'   one will create a regular slider; a numeric vector of length two will
#'   create a double-ended range slider. A warning will be issued if the
#'   value doesn't fit between \code{min} and \code{max}.
#' @param step Specifies the interval between each selectable value on the
#'   slider (\code{NULL} results a step size of 1).
#' @param round \code{TRUE} to round all values to the nearest integer;
#'   \code{FALSE} if no rounding is desired; or an integer to round to that
#'   number of digits (for example, 1 will round to the nearest 10, and -2 will
#'   round to the nearest .01). Any rounding will be applied after snapping to
#'   the nearest step.
#' @param format Deprecated.
#' @param locale Deprecated.
#' @param ticks \code{FALSE} to hide tick marks, \code{TRUE} to show them
#'   according to some simple heuristics.
#' @param animate \code{TRUE} to show simple animation controls with default
#'   settings; \code{FALSE} not to; or a custom settings list, such as those
#'   created using \code{\link{animationOptions}}.
#' @param sep Separator between thousands places in numbers.
#' @param pre A prefix string to put in front of the value.
#' @param post A suffix string to put after the value.
#' @inheritParams selectizeInput
#' @family input elements
#' @seealso \code{\link{updateSlider2Input}}
#'
#' @export
slider2Input <- function(inputId, label, min, max, value, step = NULL,
                        round = FALSE, format = NULL, locale = NULL,
                        ticks = TRUE, animate = FALSE, width = NULL, sep = ",",
                        pre = NULL, post = NULL) {

  if (!missing(format)) {
    shinyDeprecated(msg = "The `format` argument to slider2Input is deprecated. Use `sep`, `pre`, and `post` instead.",
                    version = "0.10.2")
  }
  if (!missing(locale)) {
    shinyDeprecated(msg = "The `locale` argument to slider2Input is deprecated. Use `sep`, `pre`, and `post` instead.",
                    version = "0.10.2")
  }

  # If no step size specified, use approx. 100 step points
  if (is.null(step)) {
    step <- pretty(c(min, max), n = 100)
    step <- step[2] - step[1]
  }

  # Try to get a sane number of grid marks - between 4 and 16
  if (ticks) {
    range <- max - min
    n_steps <- range / step

    # Make sure there are <= 10 steps.
    # n_ticks can be a noninteger, which is good when the range is not an
    # integer multiple of the step size, e.g., min=1, max=10, step=4
    scale_factor <- ceiling(n_steps / 10)
    n_ticks <- n_steps / scale_factor

  } else {
    n_ticks <- NULL
  }

  sliderProps <- dropNulls(list(
    class = "js-range-slider",
    id = inputId,
    `data-type` = if (length(value) > 1) "double",
    `data-min` = min,
    `data-max` = max,
    `data-from` = value[1],
    `data-to` = if (length(value) > 1) value[2],
    `data-step` = step,
    `data-grid` = ticks,
    `data-grid-num` = n_ticks,
    `data-grid-snap` = FALSE,
    `data-prettify-separator` = sep,
    `data-prefix` = pre,
    `data-postfix` = post,
    `data-keyboard` = TRUE,
    `data-keyboard-step` = step / (max - min) * 100
  ))

  # Replace any TRUE and FALSE with "true" and "false"
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) "true"
    else if (identical(x, FALSE)) "false"
    else x
  })

  sliderTag <- div(class = "form-group",
    if (!is.null(label)) controlLabel(inputId, label),
    do.call(tags$input, sliderProps)
  )


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
          span(class = 'play', icon('play', lib = 'glyphicon')),
          span(class = 'pause', icon('pause', lib = 'glyphicon'))
        )
      )
    )
  }


  dep <- htmlDependency("ionrangeslider", "2.0.1", c(href="shared/ionrangeslider"),
    script = "js/ion.rangeSlider.min.js",
    stylesheet = c("css/normalize.css", "css/ion.rangeSlider.css",
                   "css/ion.rangeSlider.skinShiny.css")
  )

  attachDependencies(sliderTag, dep)
}
