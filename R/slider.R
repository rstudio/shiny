

# Create a new slider control (list of slider input element and the script
# tag used to configure it). This is a lower level control that should
# be wrapped in an "input" construct (e.g. sliderInput in bootstrap.R)
slider <- function(inputId, min, max, value = min) {
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
  
  # build slider
  withTags({list(
    head(
      link(rel="stylesheet", 
           type="text/css", 
           href="shared/slider/css/jquery.slider.min.css"),
      
      script(src="shared/slider/js/jquery.slider.min.js")
    ),
    input(id=inputId, type="slider", 
          name=inputId, value=value),
    script(type="text/javascript",
           paste('jQuery("#', inputId, '").slider(', sep = ''),
           paste('{ from:', min, ', to:', max, sep=''), 
           '});')   
  )})
}
