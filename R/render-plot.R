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

      coordmap <<- NULL

      if (result$visible) {
        # Use capture.output to squelch printing to the actual console; we
        # are only interested in plot output

        # Special case for ggplot objects - need to capture coordmap
        if (inherits(result$value, "ggplot")) {
          capture.output(coordmap <<- getGgplotCoordmap(result$value))
        } else {
          capture.output(print(result$value))
        }
      }

      if (is.null(coordmap)) {
        coordmap <<- getPrevPlotCoordmap(width, height)
      }

      if (!is.null(coordmap)) {
        coordmap$pixelratio <<- pixelratio
      }
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

# Print a ggplot object and return a coordmap for it.
getGgplotCoordmap <- function(p) {
  if (!inherits(p, "ggplot"))
    return(NULL)

  # A modified version of print.ggplot which returns the built ggplot object
  # as well as the gtable grob.
  print_ggplot <- function(x) {
    grid::grid.newpage()

    build <- ggplot2::ggplot_build(x)

    gtable <- ggplot2::ggplot_gtable(build)
    grid::grid.draw(gtable)

    list(
      build = build,
      gtable = gtable
    )
  }

  # Given a built ggplot object, return x and y domains (in data space).
  find_panel_domains <- function(b) {
    ranges <- b$panel$ranges[[1]]
    list(
      left   = ranges$x.range[1],
      right  = ranges$x.range[2],
      bottom = ranges$y.range[1],
      top    = ranges$y.range[2]
    )
  }

  # Given a gtable object, return the x and y ranges ( in pixel dimensions)
  find_panel_dims <- function(g) {
    # Given a vector of unit objects, return logical vector indicating which ones
    # are "null" units. These units use the remaining available width/height --
    # that is, the space not occupied by elements that have an absolute size.
    is_null_unit <- function(x) {
      vapply(x, FUN.VALUE = logical(1), function(u) {
        isTRUE(attr(u, "unit", exact = TRUE) == "null")
      })
    }

    # Heights
    null_idx <- is_null_unit(g$heights)
    abs_heights <- g$heights[!null_idx]
    panel_height <- grid::convertHeight(grid::unit(1,"npc") - sum(abs_heights),
                                        "native")

    # Get all the abs heights that come before (above) the null item
    abs_heights <- g$heights[seq(1, which(null_idx) - 1)]
    above_panel_height <- grid::convertHeight(sum(abs_heights), "native")

    # Widths
    null_idx <- is_null_unit(g$widths)
    abs_widths <- g$widths[!null_idx]
    panel_width <- grid::convertWidth(grid::unit(1,"npc") - sum(abs_widths),
                                      "native")

    # Get all the abs widths that come before (left of) the null item
    abs_widths <- g$widths[seq(1, which(null_idx) - 1)]
    leftof_panel_width <- grid::convertWidth(sum(abs_widths), "native")

    top <- -as.numeric(above_panel_height)
    left <- as.numeric(leftof_panel_width)

    list(
      left = left,
      right = left + as.numeric(panel_width),
      bottom = top - as.numeric(panel_height),
      top = top
    )
  }


  res <- print_ggplot(p)

  list(
    domain = find_panel_domains(res$build),
    range = find_panel_dims(res$gtable),
    log = list(x = NULL, y = NULL)
  )
}
