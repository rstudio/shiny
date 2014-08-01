globalVariables('func')

#' Mark a function as a render function
#'
#' Should be called by implementers of \code{renderXXX} functions in order to
#' mark their return values as Shiny render functions, and to provide a hint to
#' Shiny regarding what UI function is most commonly used with this type of
#' render function. This can be used in R Markdown documents to create complete
#' output widgets out of just the render function.
#'
#' @param uiFunc A function that renders Shiny UI. Must take a single argument:
#'   an output ID.
#' @param renderFunc A function that is suitable for assigning to a Shiny output
#'   slot.
#' @return The \code{renderFunc} function, with annotations.
#'
#' @export
markRenderFunction <- function(uiFunc, renderFunc) {
  structure(renderFunc,
            class      = c("shiny.render.function", "function"),
            outputFunc = uiFunc)
}

useRenderFunction <- function(renderFunc, inline = FALSE) {
  outputFunction <- attr(renderFunc, "outputFunc")
  id <- createUniqueId(8, "out")
  o <- getDefaultReactiveDomain()$output
  if (!is.null(o))
    o[[id]] <- renderFunc
  if (is.logical(formals(outputFunction)[["inline"]])) {
    outputFunction(id, inline = inline)
  } else outputFunction(id)
}

#' @export
#' @method as.tags shiny.render.function
as.tags.shiny.render.function <- function(x, ..., inline = FALSE) {
  useRenderFunction(x, inline = inline)
}

#' Plot Output
#'
#' Renders a reactive plot that is suitable for assigning to an \code{output}
#' slot.
#'
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-plot-output}.
#'
#' @seealso For more details on how the plots are generated, and how to control
#'   the output, see \code{\link{plotPNG}}.
#'
#' @param expr An expression that generates a plot.
#' @param width,height The width/height of the rendered plot, in pixels; or
#'   \code{'auto'} to use the \code{offsetWidth}/\code{offsetHeight} of the HTML
#'   element that is bound to this plot. You can also pass in a function that
#'   returns the width/height in pixels or \code{'auto'}; in the body of the
#'   function you may reference reactive values and functions. When rendering an
#'   inline plot, you must provide numeric values (in pixels) to both
#'   \code{width} and \code{height}.
#' @param res Resolution of resulting plot, in pixels per inch. This value is
#'   passed to \code{\link{png}}. Note that this affects the resolution of PNG
#'   rendering in R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}.
#'   These can be used to set the width, height, background color, etc.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param func A function that generates a plot (deprecated; use \code{expr}
#'   instead).
#'
#' @export
renderPlot <- function(expr, width='auto', height='auto', res=72, ...,
                       env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderPlot: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  args <- list(...)

  if (is.function(width))
    widthWrapper <- reactive({ width() })
  else
    widthWrapper <- NULL

  if (is.function(height))
    heightWrapper <- reactive({ height() })
  else
    heightWrapper <- NULL

  # If renderPlot isn't going to adapt to the height of the div, then the
  # div needs to adapt to the height of renderPlot. By default, plotOutput
  # sets the height to 400px, so to make it adapt we need to override it
  # with NULL.
  outputFunc <- plotOutput
  if (!identical(height, 'auto')) formals(outputFunc)['height'] <- list(NULL)

  return(markRenderFunction(outputFunc, function(shinysession, name, ...) {
    if (!is.null(widthWrapper))
      width <- widthWrapper()
    if (!is.null(heightWrapper))
      height <- heightWrapper()

    # Note that these are reactive calls. A change to the width and height
    # will inherently cause a reactive plot to redraw (unless width and
    # height were explicitly specified).
    prefix <- 'output_'
    if (width == 'auto')
      width <- shinysession$clientData[[paste(prefix, name, '_width', sep='')]];
    if (height == 'auto')
      height <- shinysession$clientData[[paste(prefix, name, '_height', sep='')]];

    if (is.null(width) || is.null(height) || width <= 0 || height <= 0)
      return(NULL)

    # Resolution multiplier
    pixelratio <- shinysession$clientData$pixelratio
    if (is.null(pixelratio))
      pixelratio <- 1

    coordmap <- NULL
    plotFunc <- function() {
      # Actually perform the plotting
      result <- withVisible(func())
      if (result$visible) {
        # Use capture.output to squelch printing to the actual console; we
        # are only interested in plot output
        capture.output(print(result$value))
      }

      # Now capture some graphics device info before we close it
      usrCoords <- par('usr')
      usrBounds <- usrCoords
      if (par('xlog')) {
        usrBounds[c(1,2)] <- 10 ^ usrBounds[c(1,2)]
      }
      if (par('ylog')) {
        usrBounds[c(3,4)] <- 10 ^ usrBounds[c(3,4)]
      }

      coordmap <<- list(
        usr = c(
          left = usrCoords[1],
          right = usrCoords[2],
          bottom = usrCoords[3],
          top = usrCoords[4]
        ),
        # The bounds of the plot area, in DOM pixels
        bounds = c(
          left = grconvertX(usrBounds[1], 'user', 'nfc') * width,
          right = grconvertX(usrBounds[2], 'user', 'nfc') * width,
          bottom = (1-grconvertY(usrBounds[3], 'user', 'nfc')) * height,
          top = (1-grconvertY(usrBounds[4], 'user', 'nfc')) * height
        ),
        log = c(
          x = par('xlog'),
          y = par('ylog')
        ),
        pixelratio = pixelratio
      )
    }

    outfile <- do.call(plotPNG, c(plotFunc, width=width*pixelratio,
                                  height=height*pixelratio, res=res*pixelratio, args))
    on.exit(unlink(outfile))

    # Return a list of attributes for the img
    return(list(
      src=shinysession$fileUrl(name, outfile, contentType='image/png'),
      width=width, height=height, coordmap=coordmap
    ))
  }))
}

#' Image file output
#'
#' Renders a reactive image that is suitable for assigning to an \code{output}
#' slot.
#'
#' The expression \code{expr} must return a list containing the attributes for
#' the \code{img} object on the client web page. For the image to display,
#' properly, the list must have at least one entry, \code{src}, which is the
#' path to the image file. It may also useful to have a \code{contentType}
#' entry specifying the MIME type of the image. If one is not provided,
#' \code{renderImage} will try to autodetect the type, based on the file
#' extension.
#'
#' Other elements such as \code{width}, \code{height}, \code{class}, and
#' \code{alt}, can also be added to the list, and they will be used as
#' attributes in the \code{img} object.
#'
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-image-output}.
#'
#' @seealso For more details on how the images are generated, and how to control
#'   the output, see \code{\link{plotPNG}}.
#'
#' @param expr An expression that returns a list.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param deleteFile Should the file in \code{func()$src} be deleted after
#'   it is sent to the client browser? Generally speaking, if the image is a
#'   temp file generated within \code{func}, then this should be \code{TRUE};
#'   if the image is not a temp file, this should be \code{FALSE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' shinyServer(function(input, output, clientData) {
#'
#'   # A plot of fixed size
#'   output$plot1 <- renderImage({
#'     # A temp file to save the output. It will be deleted after renderImage
#'     # sends it, because deleteFile=TRUE.
#'     outfile <- tempfile(fileext='.png')
#'
#'     # Generate a png
#'     png(outfile, width=400, height=400)
#'     hist(rnorm(input$n))
#'     dev.off()
#'
#'     # Return a list
#'     list(src = outfile,
#'          alt = "This is alternate text")
#'   }, deleteFile = TRUE)
#'
#'   # A dynamically-sized plot
#'   output$plot2 <- renderImage({
#'     # Read plot2's width and height. These are reactive values, so this
#'     # expression will re-run whenever these values change.
#'     width  <- clientData$output_plot2_width
#'     height <- clientData$output_plot2_height
#'
#'     # A temp file to save the output.
#'     outfile <- tempfile(fileext='.png')
#'
#'     png(outfile, width=width, height=height)
#'     hist(rnorm(input$obs))
#'     dev.off()
#'
#'     # Return a list containing the filename
#'     list(src = outfile,
#'          width = width,
#'          height = height,
#'          alt = "This is alternate text")
#'   }, deleteFile = TRUE)
#'
#'   # Send a pre-rendered image, and don't delete the image after sending it
#'   output$plot3 <- renderImage({
#'     # When input$n is 1, filename is ./images/image1.jpeg
#'     filename <- normalizePath(file.path('./images',
#'                               paste('image', input$n, '.jpeg', sep='')))
#'
#'     # Return a list containing the filename
#'     list(src = filename)
#'   }, deleteFile = FALSE)
#' })
#'
#' }
renderImage <- function(expr, env=parent.frame(), quoted=FALSE,
                        deleteFile=TRUE) {
  installExprFunction(expr, "func", env, quoted)

  return(markRenderFunction(imageOutput, function(shinysession, name, ...) {
    imageinfo <- func()
    # Should the file be deleted after being sent? If .deleteFile not set or if
    # TRUE, then delete; otherwise don't delete.
    if (deleteFile) {
      on.exit(unlink(imageinfo$src))
    }

    # If contentType not specified, autodetect based on extension
    if (is.null(imageinfo$contentType)) {
      contentType <- getContentType(sub('^.*\\.', '', basename(imageinfo$src)))
    } else {
      contentType <- imageinfo$contentType
    }

    # Extra values are everything in imageinfo except 'src' and 'contentType'
    extra_attr <- imageinfo[!names(imageinfo) %in% c('src', 'contentType')]

    # Return a list with src, and other img attributes
    c(src = shinysession$fileUrl(name, file=imageinfo$src, contentType=contentType),
      extra_attr)
  }))
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
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(tableOutput, function() {
    classNames <- getOption('shiny.table.class') %OR% 'data table table-bordered table-condensed'
    data <- func()

    if (is.null(data) || identical(data, data.frame()))
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
#' @param width The value for \code{\link{options}('width')}.
#' @seealso \code{\link{renderText}} for displaying the value returned from a
#'   function, instead of the printed output.
#'
#' @example res/text-example.R
#'
#' @export
renderPrint <- function(expr, env = parent.frame(), quoted = FALSE, func = NULL,
                        width = getOption('width')) {
  if (!is.null(func)) {
    shinyDeprecated(msg="renderPrint: argument 'func' is deprecated. Please use 'expr' instead.")
  } else {
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(verbatimTextOutput, function() {
    op <- options(width = width)
    on.exit(options(op), add = TRUE)
    paste(capture.output(func()), collapse = "\n")
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
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(textOutput, function() {
    value <- func()
    return(paste(capture.output(cat(value)), collapse="\n"))
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
    installExprFunction(expr, "func", env, quoted)
  }

  markRenderFunction(uiOutput, function(shinysession, name, ...) {
    result <- func()
    if (is.null(result) || length(result) == 0)
      return(NULL)

    result <- takeSingletons(result, shinysession$singletons, desingleton=FALSE)$ui
    result <- surroundSingletons(result)
    dependencies <- lapply(resolveDependencies(findDependencies(result)),
      createWebDependency)
    names(dependencies) <- NULL

    # renderTags returns a list with head, singletons, and html
    output <- list(
      html = doRenderTags(result),
      deps = dependencies
    )

    return(output)
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
  return(markRenderFunction(downloadButton, function(shinysession, name, ...) {
    shinysession$registerDownload(name, filename, contentType, content)
  }))
}

#' Table output with the JavaScript library DataTables
#'
#' Makes a reactive version of the given function that returns a data frame (or
#' matrix), which will be rendered with the DataTables library. Paging,
#' searching, filtering, and sorting can be done on the R side using Shiny as
#' the server infrastructure.
#'
#' For the \code{options} argument, the character elements that have the class
#' \code{"AsIs"} (usually returned from \code{\link{I}()}) will be evaluated in
#' JavaScript. This is useful when the type of the option value is not supported
#' in JSON, e.g., a JavaScript function, which can be obtained by evaluating a
#' character string.
#' @param expr An expression that returns a data frame or a matrix.
#' @param options A list of initialization options to be passed to DataTables,
#'   or a function to return such a list.
#' @param searchDelay The delay for searching, in milliseconds (to avoid too
#'   frequent search requests).
#' @param callback A JavaScript function to be applied to the DataTable object.
#'   This is useful for DataTables plug-ins, which often require the DataTable
#'   instance to be available (\url{http://datatables.net/extensions/}).
#' @references \url{http://datatables.net}
#' @export
#' @inheritParams renderPlot
#' @examples  # pass a callback function to DataTables using I()
#' renderDataTable(iris,
#'   options = list(
#'     pageLength = 5,
#'     initComplete = I("function(settings, json) {alert('Done.');}")
#'   )
#' )
renderDataTable <- function(expr, options = NULL, searchDelay = 500,
                            callback = 'function(oTable) {}',
                            env = parent.frame(), quoted = FALSE) {
  installExprFunction(expr, "func", env, quoted)

  markRenderFunction(dataTableOutput, function(shinysession, name, ...) {
    res <- checkAsIs(if (is.function(options)) options() else options)
    data <- func()
    if (length(dim(data)) != 2) return() # expects a rectangular data object
    action <- shinysession$registerDataObj(name, data, dataTablesJSON)
    list(
      colnames = colnames(data), action = action, options = res$options,
      evalOptions = if (length(res$eval)) I(res$eval), searchDelay = searchDelay,
      callback = paste(callback, collapse = '\n')
    )
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
  renderPlot({ func() }, width=width, height=height, ...)
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
