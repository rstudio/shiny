suppressPackageStartupMessages({
  library(caTools)
  library(xtable)
})

#' Plot Output
#' 
#' Creates a reactive plot that is suitable for assigning to an \code{output} 
#' slot.
#' 
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-plot-output}.
#' 
#' @param func A function that generates a plot.
#' @param width The width of the rendered plot, in pixels; or \code{'auto'} to use
#'   the \code{offsetWidth} of the HTML element that is bound to this plot.
#' @param height The height of the rendered plot, in pixels; or \code{'auto'} to use
#'   the \code{offsetHeight} of the HTML element that is bound to this plot.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}. 
#'   These can be used to set the width, height, background color, etc.
#'   
#' @export
reactivePlot <- function(func, width='auto', height='auto', ...) {
  args <- list(...)
  
  return(function(shinyapp, name, ...) {
    png.file <- tempfile(fileext='.png')
    
    # Note that these are reactive calls. A change to the width and height
    # will inherently cause a reactive plot to redraw (unless width and 
    # height were explicitly specified).
    prefix <- '.shinyout_'
    if (width == 'auto')
      width <- shinyapp$session$get(paste(prefix, name, '_width', sep=''));
    if (height == 'auto')
      height <- shinyapp$session$get(paste(prefix, name, '_height', sep=''));
    
    if (width <= 0 || height <= 0)
      return(NULL)
    
    do.call(png, c(args, filename=png.file, width=width, height=height))
    tryCatch(
      func(),
      finally=dev.off())
    
    bytes <- file.info(png.file)$size
    if (is.na(bytes))
      return(NULL)
    
    pngData <- readBin(png.file, 'raw', n=bytes)
    if (shinyapp$allowDataUriScheme) {
      b64 <- base64encode(pngData)
      return(paste("data:image/png;base64,", b64, sep=''))
    }
    else {
      imageUrl <- shinyapp$savePlot(name, pngData, 'image/png')
      return(imageUrl)
    }
  })
}

#' Table Output
#' 
#' Creates a reactive table that is suitable for assigning to an \code{output} 
#' slot.
#' 
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{shiny-html-output}.
#' 
#' @param func A function that returns an R object that can be used with 
#'   \code{\link[xtable]{xtable}}.
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}}.
#'   
#' @export
reactiveTable <- function(func, ...) {
  reactive(function() {
    classNames <- getOption('shiny.table.class', 'data table table-bordered table-condensed')
    data <- func()

    if (is.null(data) || is.na(data))
      return("")
    
    return(paste(
      capture.output(
        print(xtable(data, ...), 
              type='html', 
              html.table.attributes=paste('class="',
                                          htmlEscape(classNames, TRUE),
                                          '"',
                                          sep=''))),
      collapse="\n"))
  })
}

#' Printable Output
#' 
#' Makes a reactive version of the given function that also turns its printable
#' result into a string. The reactive function is suitable for assigning to an 
#' \code{output} slot.
#' 
#' The corresponding HTML output tag can be anything (though \code{pre} is 
#' recommended if you need a monospace font and whitespace preserved) and should
#' have the CSS class name \code{shiny-text-output}.
#' 
#' The result of executing \code{func} will be printed inside a 
#' \code{\link[utils]{capture.output}} call.
#' 
#' @param func A function that returns a printable R object.
#'   
#' @export
reactivePrint <- function(func) {
  reactive(function() {
    return(paste(capture.output(print(func())), collapse="\n"))
  })
}

#' Text Output
#' 
#' Makes a reactive version of the given function that also uses 
#' \code{\link[base]{cat}} to turn its result into a single-element character 
#' vector.
#' 
#' The corresponding HTML output tag can be anything (though \code{pre} is 
#' recommended if you need a monospace font and whitespace preserved) and should
#' have the CSS class name \code{shiny-text-output}.
#' 
#' The result of executing \code{func} will passed to \code{cat}, inside a 
#' \code{\link[utils]{capture.output}} call.
#' 
#' @param func A function that returns an R object that can be used as an
#'   argument to \code{cat}.
#'   
#' @export
reactiveText <- function(func) {
  reactive(function() {
    return(paste(capture.output(cat(func())), collapse="\n"))
  })
}

#' UI Output
#' 
#' \bold{Experimental feature.} Makes a reactive version of a function that
#' generates HTML using the Shiny UI library.
#' 
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{shiny-html-output} (or use \code{\link{uiOutput}}).
#' 
#' @param func A function that returns a Shiny tag object, \code{\link{HTML}}, 
#'   or a list of such objects.
#'   
#' @seealso conditionalPanel
#'   
#' @export
#' @examples
#' \dontrun{
#'   output$moreControls <- reactiveUI(function() {
#'     list(
#'       
#'     )
#'   })
#' }
reactiveUI <- function(func) {
  reactive(function() {
    result <- func()
    if (is.null(result) || length(result) == 0)
      return(NULL)
    return(as.character(result))
  })
}
