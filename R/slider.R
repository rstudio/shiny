hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return (!identical(value, truncatedValue))
}

#' Animation Options
#' 
#' Creates an options object for customizing animations for \link{sliderInput}.
#' 
#' @param interval The interval, in milliseconds, between each animation step.
#' @param loop \code{TRUE} to automatically restart the animation when it 
#'   reaches the end.
#' @param playButton Specifies the appearance of the play button. Valid values 
#'   are a one-element character vector (for a simple text label), an HTML tag 
#'   or list of tags (using \code{\link{tag}} and friends), or raw HTML (using 
#'   \code{\link{HTML}}).
#' @param pauseButton Similar to \code{playButton}, but for the pause button.
#'   
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

# Create a new slider control (list of slider input element and the script
# tag used to configure it). This is a lower level control that should
# be wrapped in an "input" construct (e.g. sliderInput in bootstrap.R)
# 
# this is a wrapper for: https://github.com/egorkhmelev/jslider
# (www/shared/slider contains js, css, and img dependencies) 
slider <- function(inputId, min, max, value, step = NULL, ...,
                   round=FALSE, format='#,##0.#####', locale='us',
                   ticks=TRUE, animate=FALSE) {
  # validate inputId
  inputId <- as.character(inputId)
  if (!is.character(inputId))
    stop("inputId not specified")
    
  # validate numeric inputs
  if (!is.numeric(value) || !is.numeric(min) || !is.numeric(max)) 
    stop("min, max, amd value must all be numeric values")
  else if (min(value) < min) 
    stop(paste("slider initial value", value, 
               "is less than the specified minimum"))
  else if (max(value) > max) 
    stop(paste("slider initial value", value, 
               "is greater than the specified maximum"))
  else if (min > max) 
    stop(paste("slider maximum is greater than minimum"))
  else if (!is.null(step)) {
    if (!is.numeric(step)) 
      stop("step is not a numeric value")
    if (step > (max - min)) 
      stop("step is greater than range")
  }
  
  # step
  range <- max - min
  if (is.null(step)) {
    # short range or decimals means continuous decimal
    if (range < 2 || hasDecimals(min) || hasDecimals(max))
      step <- range / 250 # ~ one step per pixel
    else
      step = 1
  }
  
  # Default state is to not have ticks
  if (identical(ticks, TRUE)) {
    # Automatic ticks
    tickCount <- (range / step) + 1
    if (tickCount <= 26)
      ticks <- paste(rep('|', floor(tickCount)), collapse=';')
    else {
      ticks <- NULL
#       # This is a smarter auto-tick algorithm, but to be truly useful
#       # we need jslider to be able to space ticks irregularly
#       tickSize <- 10^(floor(log10(range/0.39)))
#       if ((range / tickSize) == floor(range / tickSize)) {
#         ticks <- paste(rep('|', (range / tickSize) + 1), collapse=';')
#       }
#       else {
#         ticks <- NULL
#       }
    }
  }
  else if (is.numeric(ticks) && length(ticks) == 1) {
    # Use n ticks
    ticks <- paste(rep('|', ticks), collapse=';')
  }
  else if (length(ticks) > 1 && (is.numeric(ticks) || is.character(ticks))) {
    # Explicit ticks
    ticks <- paste(ticks, collapse=';')
  }
  else {
    ticks <- NULL
  }
  
  # build slider
  sliderFragment <- list(tags$input(
    id=inputId, type="slider", 
    name=inputId, value=paste(value, collapse=';'), class="jslider",
    'data-from'=min, 'data-to'=max, 'data-step'=step,
    'data-skin'='plastic', 'data-round'=round, 'data-locale'=locale,
    'data-format'=format, 'data-scale'=ticks,
    'data-smooth'=FALSE))

  if (identical(animate, TRUE))
    animate <- animationOptions()
  
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- 'Play'
    if (is.null(animate$pauseButton))
      animate$pauseButton <- 'Pause'
    
    sliderFragment[[length(sliderFragment)+1]] <-
      tags$div(class='slider-animate-container',
               tags$a(href='#',
                      class='slider-animate-button',
                      'data-target-id'=inputId,
                      'data-interval'=animate$interval,
                      'data-loop'=animate$loop,
                      tags$span(class='play', animate$playButton),
                      tags$span(class='pause', animate$pauseButton)))
  }
  
  return(sliderFragment)
}
