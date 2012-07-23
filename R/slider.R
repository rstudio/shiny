
hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return (!identical(value, truncatedValue))
}


sliderOptions <- function(min, max, step) {
  
  # from and to
  opt <- paste('{ from:', min, ', to:', max)
  
  # step
  range <- max - min
  if (is.null(step)) {
    # short range or decimals means continuous decimal
    if (range < 2 || hasDecimals(min) || hasDecimals(max))
      step <- range / 250 # ~ one step per pixel
    else
      step = 1
  }
  opt <- paste(opt, ', step:', step)
  
  # number format
  opt <- paste(opt, ', format: { format: "#,##0.#####", locale: "us" }')
  opt <- paste(opt, ', round: 5')
  opt <- paste(opt, ', skin: "plastic"')
  opt <- paste(opt, '}')
  
  # return options
  return (opt) 
}


# Create a new slider control (list of slider input element and the script
# tag used to configure it). This is a lower level control that should
# be wrapped in an "input" construct (e.g. sliderInput in bootstrap.R)
# 
# this is a wrapper for: https://github.com/egorkhmelev/jslider
# (www/shared/slider contains js, css, and img dependencies) 
slider <- function(inputId, min, max, value, step = NULL) {
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
  
  # build slider
  list(
    tags$head(
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="shared/slider/css/jquery.slider.min.css"),
      
      tags$script(src="shared/slider/js/jquery.slider.min.js")
    ),
    tags$input(id=inputId, type="slider", 
               name=inputId, value=value),
    tags$script(type="text/javascript",
                paste('jQuery("#', inputId, '").slider(', sep = ''),
                sliderOptions(min, max, step),
                ');')
  )
}
