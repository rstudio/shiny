#' Plot Output
#'
#' Renders a reactive plot that is suitable for assigning to an \code{output}
#' slot.
#'
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-plot-output}.
#'
#' @seealso For the corresponding client-side output function, and example
#'   usage, see \code{\link{plotOutput}}. For more details on how the plots are
#'   generated, and how to control the output, see \code{\link{plotPNG}}.
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

      coordmap <<- getPrevPlotCoordmap(width, height)
      coordmap$pixelratio <- pixelratio
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

# Get a coordmap for the previous plot made with base graphics.
# Requires width and height of output image, in pixels.
# Must be called before the graphics device is closed.
getPrevPlotCoordmap <- function(width, height) {
  usrCoords <- par('usr')
  usrBounds <- usrCoords
  if (par('xlog')) {
    usrBounds[c(1,2)] <- 10 ^ usrBounds[c(1,2)]
  }
  if (par('ylog')) {
    usrBounds[c(3,4)] <- 10 ^ usrBounds[c(3,4)]
  }

  list(
    # Bounds of the plot area, in data space
    domain = list(
      left = usrCoords[1],
      right = usrCoords[2],
      bottom = usrCoords[3],
      top = usrCoords[4]
    ),
    # The bounds of the plot area, in DOM pixels
    range = list(
      left = grconvertX(usrBounds[1], 'user', 'nfc') * width - 1,
      right = grconvertX(usrBounds[2], 'user', 'nfc') * width - 1,
      bottom = (1-grconvertY(usrBounds[3], 'user', 'nfc')) * height - 1,
      top = (1-grconvertY(usrBounds[4], 'user', 'nfc')) * height - 1
    ),
    log = list(
      x = par('xlog'),
      y = par('ylog')
    )
  )
}
