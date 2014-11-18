#' @export
slider2Input <- function(inputId, label, min, max, value, step = NULL,
                        round = FALSE, format = '#,##0.#####', locale = 'us',
                        ticks = TRUE, animate = FALSE, width = NULL) {

  sliderProps <- dropNulls(list(
    class = "js-range-slider",
    id = inputId,
    `data-type` = if (length(value) > 1) "double",
    `data-min` = min,
    `data-max` = max,
    `data-from` = value[1],
    `data-to` = if (length(value) > 1) value[2],
    `data-step` = step,
    `data-grid` = if(ticks) TRUE,
    `data-grid-snap` = if (!is.null(step)) TRUE
  ))

  sliderTag <- do.call(tags$input, sliderProps)


  if (is.null(label)) {
    sliderTag <- div(class = "form-group",
      sliderTag
    )
  } else {
    sliderTag <- div(class = "form-group",
      controlLabel(inputId, label),
      sliderTag
    )
  }


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
