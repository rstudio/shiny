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

  # Wrapped in double list because other types of plots can have multiple panels.
  list(list(
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
      x = if (par('xlog')) 10 else NULL,
      y = if (par('ylog')) 10 else NULL
    ))
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

  # Given a built ggplot object and corresponding gtable, return x and y domains
  # (data space coords) and ranges (pixel coords).
  find_panel_info <- function(b) {
    layout <- b$panel$layout
    # Convert factor to numbers
    layout$PANEL <- as.integer(as.character(layout$PANEL))

    # Names of facets
    facet <- b$plot$facet
    facet_vars <- NULL
    if (inherits(facet, "grid")) {
      facet_vars <- vapply(c(facet$cols, facet$rows), as.character, character(1))
    } else if (inherits(facet, "wrap")) {
      facet_vars <- vapply(b$plot$facet$facets, as.character, character(1))
    }

    # Iterate over each row in the layout data frame
    lapply(seq_len(nrow(layout)), function(i) {
      # Slice out one row
      l <- layout[i, ]

      scale_x <- l$SCALE_X
      scale_y <- l$SCALE_Y

      vars <- lapply(facet_vars, function(var) {
        list(name = var, value = l[[var]])
      })

      list(
        panel   = l$PANEL,
        row     = l$ROW,
        col     = l$COL,
        vars    = vars,
        scale_x = scale_x,
        scale_y = scale_x,
        log     = check_log_scales(b, scale_x, scale_y),
        domain  = find_panel_domain(b, l$PANEL, scale_x, scale_y)
      )
    })
  }

  # Given a single range object (representing the data domain) from a built
  # ggplot object, return the domain.
  find_panel_domain <- function(b, panel_num, scalex_num = 1, scaley_num = 1) {
    range <- b$panel$ranges[[panel_num]]
    res <- list(
      left   = range$x.range[1],
      right  = range$x.range[2],
      bottom = range$y.range[1],
      top    = range$y.range[2]
    )

    # Check for reversed scales
    xscale <- b$panel$x_scales[[scalex_num]]
    yscale <- b$panel$y_scales[[scaley_num]]

    if (!is.null(xscale$trans) && xscale$trans$name == "reverse") {
      res$left  <- -res$left
      res$right <- -res$right
    }
    if (!is.null(yscale$trans) && yscale$trans$name == "reverse") {
      res$top    <- -res$top
      res$bottom <- -res$bottom
    }

    res
  }

  # Given built ggplot object, return object with the log base for x and y if
  # there are log scales or coord transforms.
  check_log_scales <- function(b, scalex_num = 1, scaley_num = 1) {

    # Given a vector of transformation names like c("log-10", "identity"),
    # return the first log base, like 10. If none are present, return NULL.
    extract_log_base <- function(names) {
      names <- names[grepl("^log-", names)]

      if (length(names) == 0)
        return(NULL)

      names <- names[1]

      as.numeric(sub("^log-", "", names))
    }

    # Look for log scales and log coord transforms. People shouldn't use both.
    x_names <- character(0)
    y_names <- character(0)

    # Continuous scales have a trans; discrete ones don't
    if (!is.null(b$panel$x_scales[[scalex_num]]$trans))
      x_names <- b$panel$x_scales[[scalex_num]]$trans$name
    if (!is.null(b$panel$y_scales[[scaley_num]]$trans))
      y_names <- b$panel$y_scales[[scaley_num]]$trans$name

    coords <- b$plot$coordinates
    if (!is.null(coords$trans)) {
      if (!is.null(coords$trans$x))
        x_names <- c(x_names, coords$trans$x$name)
      if (!is.null(coords$trans$y))
        y_names <- c(y_names, coords$trans$y$name)
    }

    # Keep only scale/trans names that start with "log-"
    x_names <- x_names[grepl("^log-", x_names)]
    y_names <- y_names[grepl("^log-", y_names)]

    # Extract the log base from the trans name -- a string like "log-10".
    list(
      x = extract_log_base(x_names),
      y = extract_log_base(y_names)
    )
  }

  # Given a gtable object, return the x and y ranges (in pixel dimensions)
  find_panel_ranges <- function(g) {
    # Given a vector of unit objects, return logical vector indicating which ones
    # are "null" units. These units use the remaining available width/height --
    # that is, the space not occupied by elements that have an absolute size.
    is_null_unit <- function(x) {
      vapply(x, FUN.VALUE = logical(1), function(u) {
        isTRUE(attr(u, "unit", exact = TRUE) == "null")
      })
    }

    # Convert a unit (or vector of units) to a numeric vector of pixel sizes
    h_px <- function(x) as.numeric(grid::convertHeight(x, "native"))
    w_px <- function(x) as.numeric(grid::convertWidth(x, "native"))

    # Pixel dimensions of image
    img_height <- h_px(grid::unit(1, "npc"))
    img_width  <- w_px(grid::unit(1, "npc"))

    # Calculate height of all panel(s) together. Panels (and only panels) have
    # null height.
    null_idx <- is_null_unit(g$heights)
    # All the absolute heights. At this point, null heights are 0. We need to
    # calculate them separately and add them in later.
    abs_heights <- h_px(g$heights)
    panel_height_total <- img_height - sum(abs_heights)
    # Divide up the total panel height up into the panels (scaled by height)
    panel_height_prop <- as.numeric(g$heights[null_idx])
    panel_height_prop <- panel_height_prop / sum(panel_height_prop)
    abs_heights[null_idx] <- panel_height_total * panel_height_prop
    abs_heights <- abs(abs_heights)

    # Same for widths
    null_idx <- is_null_unit(g$widths)
    abs_widths <- w_px(g$widths)
    panel_width_total <- img_width - sum(abs_widths)
    panel_width_prop <- as.numeric(g$widths[null_idx])
    panel_width_prop <- panel_width_prop / sum(panel_width_prop)
    abs_widths[null_idx] <- panel_width_total * panel_width_prop
    abs_widths <- abs(abs_widths)

    # Convert to absolute pixel positions
    x_pos <- cumsum(abs_widths)
    y_pos <- cumsum(abs_heights)

    # Get panel names
    layout <- g$layout
    # If multiple panels, they'll be named "panel-1", "panel-2", etc. If just
    # one, it'll be named "panel". Rename it to "panel-1" to keep things simple.
    layout$name[layout$name == "panel"] <- "panel-1"
    layout <- layout[grepl("^panel", layout$name), ]
    layout$panel <- as.integer(sub("^panel", "", layout$name))

    # Return list of lists, where each inner list has left, right, top, bottom
    # values for a panel
    lapply(seq_len(nrow(layout)), function(i) {
      p <- layout[i, , drop = FALSE]
      list(
        left   = x_pos[p$l - 1],
        right  = x_pos[p$r],
        bottom = y_pos[p$b],
        top    = y_pos[p$t - 1]
      )
    })
  }


  res <- print_ggplot(p)

  # Get info from built ggplot object
  info <- find_panel_info(res$build)

  # Get ranges from gtable - it's possible for this to return more elements than
  # info, because it calculates positions even for panels that aren't present.
  # This can happen with facet_wrap.
  ranges <- find_panel_ranges(res$gtable)

  for (i in seq_along(info)) {
    info[[i]]$range <- ranges[[i]]
  }

  info
}
