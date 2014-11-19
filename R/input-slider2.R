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
    `data-postfix` = post
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
