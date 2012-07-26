hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return (!identical(value, truncatedValue))
}

# Create a new slider control (list of slider input element and the script
# tag used to configure it). This is a lower level control that should
# be wrapped in an "input" construct (e.g. sliderInput in bootstrap.R)
# 
# this is a wrapper for: https://github.com/egorkhmelev/jslider
# (www/shared/slider contains js, css, and img dependencies) 
slider <- function(inputId, min, max, value, step = NULL, round = NULL,
                   locale='us', format='#,##0.#####', ticks=TRUE) {
  # validate inputId
  inputId <- as.character(inputId)
  if (!is.character(inputId))
    stop("inputId not specified")
    
  # validate numeric inputs
  if (!is.numeric(value) || !is.numeric(min) || !is.numeric(max)) 
    stop("min, max, amd value must all be numeric values")
  else if (value < min) 
    stop(paste("slider initial value", value, 
               "is less than the specified minimum"))
  else if (value > max) 
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
  if (identical(ticks, T)) {
    # Automatic ticks
    tickCount <- range / step
    if (tickCount <= 25)
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
  list(
    tags$head(
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="shared/slider/css/jquery.slider.min.css"),
      
      tags$script(src="shared/slider/js/jquery.slider.min.js")
    ),
    tags$input(id=inputId, type="slider", 
               name=inputId, value=value, class="jslider",
               'data-from'=min, 'data-to'=max, 'data-step'=step,
               'data-skin'='plastic', 'data-round'=round, 'data-locale'=locale,
               'data-format'=format, 'data-scale'=ticks),
    tags$script(type="text/javascript",
                paste('jQuery("#', inputId, '").slider();', sep = ''))
  )
}
