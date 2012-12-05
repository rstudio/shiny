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
#' For output, it will try to use the following devices, in this order:
#' quartz (via \code{\link[grDevices]{png}}), then \code{\link[Cairo]{CairoPNG}},
#' and finally \code{\link[grDevices]{png}}. This is in order of quality of
#' output. Notably, plain \code{png} output on Linux and Windows may not
#' antialias some point shapes, resulting in poor quality output.
#' 
#' @param func A function that generates a plot.
#' @param width The width of the rendered plot, in pixels; or \code{'auto'} to 
#'   use the \code{offsetWidth} of the HTML element that is bound to this plot. 
#'   You can also pass in a function that returns the width in pixels or 
#'   \code{'auto'}; in the body of the function you may reference reactive 
#'   values and functions.
#' @param height The height of the rendered plot, in pixels; or \code{'auto'} to
#'   use the \code{offsetHeight} of the HTML element that is bound to this plot.
#'   You can also pass in a function that returns the width in pixels or 
#'   \code{'auto'}; in the body of the function you may reference reactive 
#'   values and functions.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}. 
#'   These can be used to set the width, height, background color, etc.
#'   
#' @export
reactivePlot <- function(func, width='auto', height='auto', ...) {
  args <- list(...)
  
  if (is.function(width))
    width <- reactive(width)
  if (is.function(height))
    height <- reactive(height)

  return(function(shinyapp, name, ...) {
    png.file <- tempfile(fileext='.png')
    
    if (is.function(width))
      width <- width()
    if (is.function(height))
      height <- height()
    
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

    # If quartz is available, use png() (which will default to quartz).
    # Otherwise, if the Cairo package is installed, use CairoPNG().
    # Finally, if neither quartz nor Cairo, use png().
    if (capabilities("aqua"))
      pngfun <- png
    else if (nchar(system.file(package = "Cairo")))
      pngfun <- Cairo::CairoPNG
    else
      pngfun <- png

    do.call(pngfun, c(args, filename=png.file, width=width, height=height))
    on.exit(unlink(png.file))
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
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}} and
#'   \code{\link[xtable]{print.xtable}}.
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
                                          sep=''), ...)),
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
    # Wrap result in tagList in case it is an ordinary list
    return(as.character(tagList(result)))
  })
}

#' File Downloads
#' 
#' Allows content from the Shiny application to be made available to the user as
#' file downloads (for example, downloading the currently visible data as a CSV 
#' file). Both filename and contents can be calculated dynamically at the time 
#' the user initiates the download. Assign the return value to a slot on 
#' \code{output} in your server function, and in the UI use 
#' \code{\link{downloadButton}} or \code{\link{downloadLink}} to make the
#' download available.
#' 
#' @param filename A string of the filename, including extension, that the 
#'   user's web browser should default to when downloading the file; or a 
#'   function that returns such a string. (Reactive values and functions may be 
#'   used from this function.)
#' @param content A function that takes a single argument \code{file} that is a 
#'   file path (string) of a nonexistent temp file, and writes the content to
#'   that file path. (Reactive values and functions may be used from this
#'   function.)
#' @param contentType A string of the download's 
#'   \href{http://en.wikipedia.org/wiki/Internet_media_type}{content type}, for 
#'   example \code{"text/csv"} or \code{"image/png"}. If \code{NULL} or 
#'   \code{NA}, the content type will be guessed based on the filename 
#'   extension, or \code{application/octet-stream} if the extension is unknown.
#'   
#' @examples
#' \dontrun{
#' # In server.R:
#' output$downloadData <- downloadHandler(
#'   filename = function() {
#'     paste('data-', Sys.Date(), '.csv', sep='')
#'   },
#'   content = function(file) {
#'     write.csv(data, file)
#'   }
#' )
#' 
#' # In ui.R:
#' downloadLink('downloadData', 'Download')
#' }
#' 
#' @export
downloadHandler <- function(filename, content, contentType=NA) {
  return(function(shinyapp, name, ...) {
    shinyapp$registerDownload(name, filename, contentType, content)
  })
}