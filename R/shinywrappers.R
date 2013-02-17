suppressPackageStartupMessages({
  library(caTools)
  library(xtable)
})

#' Plot Output
#' 
#' Renders a reactive plot that is suitable for assigning to an \code{output} 
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
#' @param expr An expression that generates a plot.
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
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that generates a plot (deprecated; use \code{expr}
#'   instead).
#'   
#' @export
renderPlot <- function(expr, width='auto', height='auto', ...,
                       env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderPlot: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    func <- exprToFunction(expr, env, quoted)
  }


  args <- list(...)
  
  if (is.function(width))
    width <- reactive({ width() })
  if (is.function(height))
    height <- reactive({ height() })

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
    if (capabilities("aqua")) {
      pngfun <- png
    } else if (nchar(system.file(package = "Cairo"))) {
      require(Cairo)
      pngfun <- CairoPNG
    } else {
      pngfun <- png
    }

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
#' @param expr An expression that returns an R object that can be used with 
#'   \code{\link[xtable]{xtable}}.
#' @param ... Arguments to be passed through to \code{\link[xtable]{xtable}} and
#'   \code{\link[xtable]{print.xtable}}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used with 
#'   \code{\link[xtable]{xtable}} (deprecated; use \code{expr} instead).
#'   
#' @export
renderTable <- function(expr, ..., env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderTable: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    func <- exprToFunction(expr, env, quoted)
  }

  function() {
    classNames <- getOption('shiny.table.class', 'data table table-bordered table-condensed')
    data <- func()

    if (is.null(data))
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
  }
}

#' Printable Output
#' 
#' Makes a reactive version of the given function that captures any printed 
#' output, and also captures its printable result (unless 
#' \code{\link{invisible}}), into a string. The resulting function is suitable 
#' for assigning to an  \code{output} slot.
#' 
#' The corresponding HTML output tag can be anything (though \code{pre} is 
#' recommended if you need a monospace font and whitespace preserved) and should
#' have the CSS class name \code{shiny-text-output}.
#' 
#' The result of executing \code{func} will be printed inside a 
#' \code{\link[utils]{capture.output}} call.
#' 
#' Note that unlike most other Shiny output functions, if the given function 
#' returns \code{NULL} then \code{NULL} will actually be visible in the output. 
#' To display nothing, make your function return \code{\link{invisible}()}.
#' 
#' @param expr An expression that may print output and/or return a printable R 
#'   object.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' @param func A function that may print output and/or return a printable R 
#'   object (deprecated; use \code{expr} instead).
#'   
#' @seealso \code{\link{renderText}} for displaying the value returned from a 
#'   function, instead of the printed output.
#'
#' @example res/text-example.R
#'   
#' @export
renderPrint <- function(expr, env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderPrint: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    func <- exprToFunction(expr, env, quoted)
  }

  function() {
    return(paste(capture.output({
      result <- withVisible(func())
      if (result$visible)
        print(result$value)
    }), collapse="\n"))
  }
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
#' @param expr An expression that returns an R object that can be used as an
#'   argument to \code{cat}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns an R object that can be used as an
#'   argument to \code{cat}.(deprecated; use \code{expr} instead).
#'   
#' @seealso \code{\link{renderPrint}} for capturing the print output of a
#'   function, rather than the returned text value.
#'
#' @example res/text-example.R
#'   
#' @export
renderText <- function(expr, env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderText: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    func <- exprToFunction(expr, env, quoted)
  }

  function() {
    value <- func()
    return(paste(capture.output(cat(value)), collapse="\n"))
  }
}

#' UI Output
#' 
#' \bold{Experimental feature.} Makes a reactive version of a function that
#' generates HTML using the Shiny UI library.
#' 
#' The corresponding HTML output tag should be \code{div} and have the CSS class
#' name \code{shiny-html-output} (or use \code{\link{uiOutput}}).
#' 
#' @param expr An expression that returns a Shiny tag object, \code{\link{HTML}}, 
#'   or a list of such objects.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that returns a Shiny tag object, \code{\link{HTML}}, 
#'   or a list of such objects (deprecated; use \code{expr} instead).
#'   
#' @seealso conditionalPanel
#'   
#' @export
#' @examples
#' \dontrun{
#'   output$moreControls <- renderUI({
#'     list(
#'       
#'     )
#'   })
#' }
renderUI <- function(expr, env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderUI: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    func <- exprToFunction(expr, env, quoted)
  }

  function() {
    result <- func()
    if (is.null(result) || length(result) == 0)
      return(NULL)
    # Wrap result in tagList in case it is an ordinary list
    return(as.character(tagList(result)))
  }
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


# Deprecated functions ------------------------------------------------------

#' Plot output (deprecated)
#'
#' See \code{\link{renderPlot}}.
#' @param func A function.
#' @param width Width.
#' @param height Height.
#' @param ... Other arguments to pass on.
#' @export
reactivePlot <- function(func, width='auto', height='auto', ...) {
  shinyDeprecated(new="renderPlot")
  renderPlot({ func() }, width='auto', height='auto', ...)
}

#' Table output (deprecated)
#'
#' See \code{\link{renderTable}}.
#' @param func A function.
#' @param ... Other arguments to pass on.
#' @export
reactiveTable <- function(func, ...) {
  shinyDeprecated(new="renderTable")
  renderTable({ func() })
}

#' Print output (deprecated)
#'
#' See \code{\link{renderPrint}}.
#' @param func A function.
#' @export
reactivePrint <- function(func) {
  shinyDeprecated(new="renderPrint")
  renderPrint({ func() })
}

#' UI output (deprecated)
#'
#' See \code{\link{renderUI}}.
#' @param func A function.
#' @export
reactiveUI <- function(func) {
  shinyDeprecated(new="renderUI")
  renderUI({ func() })
}

#' Text output (deprecated)
#'
#' See \code{\link{renderText}}.
#' @param func A function.
#' @export
reactiveText <- function(func) {
  shinyDeprecated(new="renderText")
  renderText({ func() })
}
