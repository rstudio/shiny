suppressPackageStartupMessages({
  library(caTools)
  library(xtable)
})

#' Creates a reactive plot that is suitable for assigning to an \code{output} 
#' slot.
#' 
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{live-plot}.
#' 
#' @param func A function that generates a plot.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}. 
#'   These can be used to set the width, height, background color, etc.
#'   
#' @export
reactivePlot <- function(func, ...) {
  reactive(function() {
    png.file <- tempfile(fileext='.png')
    png(filename=png.file, ...)
    tryCatch(
      func(),
      finally=dev.off())
    
    bytes <- file.info(png.file)$size
    if (is.na(bytes))
      return(NULL)
    
    b64 <- base64encode(readBin(png.file, 'raw', n=bytes))
    return(paste("data:image/png;base64,", b64, sep=''))
  })
}

#' Creates a reactive table that is suitable for assigning to an \code{output} 
#' slot.
#' 
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{live-html}.
#' 
#' @param func A function that returns an R object that can be used with 
#'   \code{\link[xtable]{xtable}}.
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}}.
#'   
#' @export
reactiveTable <- function(func, ...) {
  reactive(function() {
    data <- func()
    return(paste(
      capture.output(
        print(xtable(data, ...), 
              type='html', 
              html.table.attributes='class="data"')),
      collapse="\n"))
  })
}

#' Creates a reactive text value that is suitable for assigning to an 
#' \code{output} slot.
#' 
#' The corresponding HTML output tag can be anything (though \code{pre} is 
#' recommended if you need a monospace font and whitespace preserved) and should
#' have the CSS class name \code{live-text}.
#' 
#' The result of executing \code{func} will be printed inside a 
#' \code{\link[utils]{capture.output}} call. If instead you want to return a
#' single-element character vector to be used verbatim, return it wrapped in 
#' \code{\link[base]{invisible}} (or alternatively, just use
#' \code{\link{reactive}} instead of \code{reactiveText}.
#' 
#' @param func A function that returns a printable R object, or an invisible 
#'   character vector.
#'   
#' @export
reactiveText <- function(func) {
  reactive(function() {
    x <- withVisible(func())
    if (x$visible)
      return(paste(capture.output(print(x$value)), collapse="\n"))
    else
      return(x)
  })
}
