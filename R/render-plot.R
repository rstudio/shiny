#' Plot Output
#'
#' Renders a reactive plot that is suitable for assigning to an \code{output}
#' slot.
#'
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-plot-output}.
#'
#' @section Interactive plots:
#'
#'   With ggplot2 graphics, the code in \code{renderPlot} should return a ggplot
#'   object; if instead the code prints the ggplot2 object with something like
#'   \code{print(p)}, then the coordinates for interactive graphics will not be
#'   properly scaled to the data space.
#'
#'   See \code{\link{plotOutput}} for more information about interactive plots.
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
  # This ..stacktraceon is matched by a ..stacktraceoff.. when plotFunc
  # is called
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)

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
          utils::capture.output(coordmap <<- getGgplotCoordmap(result$value, pixelratio))
        } else {
          # This ..stacktraceon.. negates the ..stacktraceoff.. that wraps the
          # call to plotFunc
          utils::capture.output(..stacktraceon..(print(result$value)))
        }
      }

      if (is.null(coordmap)) {
        coordmap <<- getPrevPlotCoordmap(width, height)
      }
    }

    # This ..stacktraceoff.. is matched by the `func` function's
    # wrapFunctionLabel(..stacktraceon=TRUE) call near the beginning of
    # renderPlot, and by the ..stacktraceon.. in plotFunc where ggplot objects
    # are printed
    outfile <- ..stacktraceoff..(
      do.call(plotPNG, c(plotFunc, width=width*pixelratio,
        height=height*pixelratio, res=res*pixelratio, args))
    )
    on.exit(unlink(outfile))

    # A list of attributes for the img
    res <- list(
      src=shinysession$fileUrl(name, outfile, contentType='image/png'),
      width=width, height=height, coordmap=coordmap
    )

    # Get error message if present (from attribute on the coordmap)
    error <- attr(coordmap, "error", exact = TRUE)
    if (!is.null(error)) {
      res$error <- error
    }

    res
  }))
}

# The coordmap extraction functions below return something like the examples
# below. For base graphics:
# plot(mtcars$wt, mtcars$mpg)
# str(getPrevPlotCoordmap(400, 300))
# List of 1
#  $ :List of 4
#   ..$ domain :List of 4
#   .. ..$ left  : num 1.36
#   .. ..$ right : num 5.58
#   .. ..$ bottom: num 9.46
#   .. ..$ top   : num 34.8
#   ..$ range  :List of 4
#   .. ..$ left  : num 50.4
#   .. ..$ right : num 373
#   .. ..$ bottom: num 199
#   .. ..$ top   : num 79.6
#   ..$ log    :List of 2
#   .. ..$ x: NULL
#   .. ..$ y: NULL
#   ..$ mapping: Named list()
#
# For ggplot2, it might be something like:
# p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# str(getGgplotCoordmap(p, 1))
# List of 1
#  $ :List of 10
#   ..$ panel     : int 1
#   ..$ row       : int 1
#   ..$ col       : int 1
#   ..$ panel_vars: Named list()
#   ..$ scale_x   : int 1
#   ..$ scale_y   : int 1
#   ..$ log       :List of 2
#   .. ..$ x: NULL
#   .. ..$ y: NULL
#   ..$ domain    :List of 4
#   .. ..$ left  : num 1.32
#   .. ..$ right : num 5.62
#   .. ..$ bottom: num 9.22
#   .. ..$ top   : num 35.1
#   ..$ mapping   :List of 2
#   .. ..$ x: chr "wt"
#   .. ..$ y: chr "mpg"
#   ..$ range     :List of 4
#   .. ..$ left  : num 40.8
#   .. ..$ right : num 446
#   .. ..$ bottom: num 263
#   .. ..$ top   : num 14.4
#
# With a faceted ggplot2 plot, the outer list contains two objects, each of
# which represents one panel. In this example, there is one panelvar, but there
# can be up to two of them.
# mtc <- mtcars
# mtc$am <- factor(mtc$am)
# p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ am)
# str(getGgplotCoordmap(p, 1))
# List of 2
#  $ :List of 10
#   ..$ panel     : int 1
#   ..$ row       : int 1
#   ..$ col       : int 1
#   ..$ panel_vars:List of 1
#   .. ..$ panelvar1: Factor w/ 2 levels "0","1": 1
#   ..$ scale_x   : int 1
#   ..$ scale_y   : int 1
#   ..$ log       :List of 2
#   .. ..$ x: NULL
#   .. ..$ y: NULL
#   ..$ domain    :List of 4
#   .. ..$ left  : num 1.32
#   .. ..$ right : num 5.62
#   .. ..$ bottom: num 9.22
#   .. ..$ top   : num 35.1
#   ..$ mapping   :List of 3
#   .. ..$ x        : chr "wt"
#   .. ..$ y        : chr "mpg"
#   .. ..$ panelvar1: chr "am"
#   ..$ range     :List of 4
#   .. ..$ left  : num 45.6
#   .. ..$ right : num 317
#   .. ..$ bottom: num 251
#   .. ..$ top   : num 35.7
#  $ :List of 10
#   ..$ panel     : int 2
#   ..$ row       : int 1
#   ..$ col       : int 2
#   ..$ panel_vars:List of 1
#   .. ..$ panelvar1: Factor w/ 2 levels "0","1": 2
#   ..$ scale_x   : int 1
#   ..$ scale_y   : int 1
#   ..$ log       :List of 2
#   .. ..$ x: NULL
#   .. ..$ y: NULL
#   ..$ domain    :List of 4
#   .. ..$ left  : num 1.32
#   .. ..$ right : num 5.62
#   .. ..$ bottom: num 9.22
#   .. ..$ top   : num 35.1
#   ..$ mapping   :List of 3
#   .. ..$ x        : chr "wt"
#   .. ..$ y        : chr "mpg"
#   .. ..$ panelvar1: chr "am"
#   ..$ range     :List of 4
#   .. ..$ left  : num 322
#   .. ..$ right : num 594
#   .. ..$ bottom: num 251
#   .. ..$ top   : num 35.7


# Get a coordmap for the previous plot made with base graphics.
# Requires width and height of output image, in pixels.
# Must be called before the graphics device is closed.
getPrevPlotCoordmap <- function(width, height) {
  usrCoords <- graphics::par('usr')
  usrBounds <- usrCoords
  if (graphics::par('xlog')) {
    usrBounds[c(1,2)] <- 10 ^ usrBounds[c(1,2)]
  }
  if (graphics::par('ylog')) {
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
      left = graphics::grconvertX(usrBounds[1], 'user', 'nfc') * width,
      right = graphics::grconvertX(usrBounds[2], 'user', 'nfc') * width,
      bottom = (1-graphics::grconvertY(usrBounds[3], 'user', 'nfc')) * height - 1,
      top = (1-graphics::grconvertY(usrBounds[4], 'user', 'nfc')) * height - 1
    ),
    log = list(
      x = if (graphics::par('xlog')) 10 else NULL,
      y = if (graphics::par('ylog')) 10 else NULL
    ),
    # We can't extract the original variable names from a base graphic.
    # `mapping` is an empty _named_ list, so that it is converted to an object
    # (not an array) in JSON.
    mapping = list(x = NULL)[0]
  ))
}

# Print a ggplot object and return a coordmap for it.
getGgplotCoordmap <- function(p, pixelratio) {
  if (!inherits(p, "ggplot"))
    return(NULL)

  # A modified version of print.ggplot which returns the built ggplot object as
  # well as the gtable grob. This overrides the ggplot::print.ggplot method, but
  # only within the context of getGgplotCoordmap. The reason this needs to be an
  # (pseudo) S3 method is so that, if an object has a class in addition to
  # ggplot, and there's a print method for that class, that we won't override
  # that method.
  # https://github.com/rstudio/shiny/issues/841
  print.ggplot <- function(x) {
    grid::grid.newpage()

    build <- ggplot2::ggplot_build(x)

    gtable <- ggplot2::ggplot_gtable(build)
    grid::grid.draw(gtable)

    list(
      build = build,
      gtable = gtable
    )
  }

  # Given the name of a generic function and an object, return the class name
  # for the method that would be used on the object.
  which_method <- function(generic, x) {
    classes <- class(x)
    method_names <- paste(generic, classes, sep = ".")
    idx <- which(method_names %in% utils::methods(generic))

    if (length(idx) == 0)
      return(NULL)

    # Return name of first class with matching method
    classes[idx[1]]
  }


  # Given a built ggplot object, return x and y domains (data space coords) for
  # each panel.
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
      facet_vars <- vapply(facet$facets, as.character, character(1))
    }

    # Iterate over each row in the layout data frame
    lapply(seq_len(nrow(layout)), function(i) {
      # Slice out one row
      l <- layout[i, ]

      scale_x <- l$SCALE_X
      scale_y <- l$SCALE_Y

      mapping <- find_plot_mappings(b)

      # For each of the faceting variables, get the value of that variable in
      # the current panel. Default to empty _named_ list so that it's sent as a
      # JSON object, not array.
      panel_vars <- list(a = NULL)[0]
      for (i in seq_along(facet_vars)) {
        var_name <- facet_vars[[i]]
        vname <- paste0("panelvar", i)

        mapping[[vname]] <- var_name
        panel_vars[[vname]] <- l[[var_name]]
      }

      list(
        panel   = l$PANEL,
        row     = l$ROW,
        col     = l$COL,
        panel_vars = panel_vars,
        scale_x = scale_x,
        scale_y = scale_x,
        log     = check_log_scales(b, scale_x, scale_y),
        domain  = find_panel_domain(b, l$PANEL, scale_x, scale_y),
        mapping = mapping
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

  # Given a built ggplot object, return a named list of variables mapped to x
  # and y. This function will be called for each panel, but in practice the
  # result is always the same across panels, so we'll cache the result.
  mappings_cache <- NULL
  find_plot_mappings <- function(b) {
    if (!is.null(mappings_cache))
      return(mappings_cache)

    # lapply'ing as.character results in unexpected behavior for expressions
    # like `wt/2`. This works better.
    mappings <- as.list(as.character(b$plot$mapping))

    # If x or y mapping is missing, look in each layer for mappings and return
    # the first one.
    missing_mappings <- setdiff(c("x", "y"), names(mappings))
    if (length(missing_mappings) != 0) {
      # Grab mappings for each layer
      layer_mappings <- lapply(b$plot$layers, function(layer) {
        lapply(layer$mapping, as.character)
      })

      # Get just the first x or y value in the combined list of plot and layer
      # mappings.
      mappings <- c(list(mappings), layer_mappings)
      mappings <- Reduce(x = mappings, init = list(x = NULL, y = NULL),
        function(init, m) {
          if (is.null(init$x) && !is.null(m$x)) init$x <- m$x
          if (is.null(init$y) && !is.null(m$y)) init$y <- m$y
          init
        }
      )
    }

    mappings_cache <<- mappings
    mappings
  }

  # Given a gtable object, return the x and y ranges (in pixel dimensions)
  find_panel_ranges <- function(g, pixelratio) {
    # Given a vector of unit objects, return logical vector indicating which ones
    # are "null" units. These units use the remaining available width/height --
    # that is, the space not occupied by elements that have an absolute size.
    is_null_unit <- function(x) {
      # A vector of units can be either a list of individual units (a unit.list
      # object), each with their own set of attributes, or an atomic vector with
      # one set of attributes. ggplot2 switched from the former (in version
      # 1.0.1) to the latter. We need to make sure that we get the correct
      # result in both cases.
      if (inherits(x, "unit.list")) {
        # For ggplot2 <= 1.0.1
        vapply(x, FUN.VALUE = logical(1), function(u) {
          isTRUE(attr(u, "unit", exact = TRUE) == "null")
        })
      } else {
        # For later versions of ggplot2
        attr(x, "unit", exact = TRUE) == "null"
      }
    }

    # Convert a unit (or vector of units) to a numeric vector of pixel sizes
    h_px <- function(x) as.numeric(grid::convertHeight(x, "native"))
    w_px <- function(x) as.numeric(grid::convertWidth(x, "native"))

    # Given a vector of relative sizes (in grid units), and a function for
    # converting grid units to numeric pixels, return a numeric vector of
    # pixel sizes.
    find_px_sizes <- function(rel_sizes, unit_to_px) {
      # Total pixels (in height or width)
      total_px <- unit_to_px(grid::unit(1, "npc"))
      # Calculate size of all panel(s) together. Panels (and only panels) have
      # null size.
      null_idx <- is_null_unit(rel_sizes)
      # All the absolute heights. At this point, null heights are 0. We need to
      # calculate them separately and add them in later.
      px_sizes <- unit_to_px(rel_sizes)
      # Total size for panels is image size minus absolute (non-panel) elements
      panel_px_total <- total_px - sum(px_sizes)
      # Divide up the total panel size up into the panels (scaled by size)
      panel_sizes_rel <- as.numeric(rel_sizes[null_idx])
      panel_sizes_rel <- panel_sizes_rel / sum(panel_sizes_rel)
      px_sizes[null_idx] <- panel_px_total * panel_sizes_rel
      abs(px_sizes)
    }

    px_heights <- find_px_sizes(g$heights, h_px)
    px_widths <- find_px_sizes(g$widths, w_px)

    # Convert to absolute pixel positions
    x_pos <- cumsum(px_widths)
    y_pos <- cumsum(px_heights)

    # Match up the pixel dimensions to panels
    layout <- g$layout
    # For panels:
    # * For facet_wrap, they'll be named "panel-1", "panel-2", etc.
    # * For no facet or facet_grid, they'll just be named "panel". For
    #   facet_grid, we need to re-order the layout table. Assume that panel
    #   numbers go from left to right, then next row.
    # Assign a number to each panel, corresponding to PANEl in the built ggplot
    # object.
    layout <- layout[grepl("^panel", layout$name), ]
    layout <- layout[order(layout$t, layout$l), ]
    layout$panel <- seq_len(nrow(layout))

    # When using a HiDPI client on a Linux server, the pixel
    # dimensions are doubled, so we have to divide the dimensions by
    # `pixelratio`. When a HiDPI client is used on a Mac server (with
    # the quartz device), the pixel dimensions _aren't_ doubled, even though
    # the image has double size. In the latter case we don't have to scale the
    # numbers down.
    pix_ratio <- 1
    if (!grepl("^quartz", names(grDevices::dev.cur()))) {
      pix_ratio <- pixelratio
    }

    # Return list of lists, where each inner list has left, right, top, bottom
    # values for a panel
    lapply(seq_len(nrow(layout)), function(i) {
      p <- layout[i, , drop = FALSE]
      list(
        left   = x_pos[p$l - 1] / pix_ratio,
        right  = x_pos[p$r] / pix_ratio,
        bottom = y_pos[p$b] / pix_ratio,
        top    = y_pos[p$t - 1] / pix_ratio
      )
    })
  }

  # If print(p) gets dispatched to print.ggplot(p), attempt to extract coordmap.
  # If dispatched to another method, just print the object and don't attempt to
  # extract the coordmap. This can happen if there's another print method that
  # takes precedence.
  if (identical(which_method("print", p), "ggplot")) {
    res <- print(p)

    tryCatch({
      # Get info from built ggplot object
      info <- find_panel_info(res$build)

      # Get ranges from gtable - it's possible for this to return more elements than
      # info, because it calculates positions even for panels that aren't present.
      # This can happen with facet_wrap.
      ranges <- find_panel_ranges(res$gtable, pixelratio)

      for (i in seq_along(info)) {
        info[[i]]$range <- ranges[[i]]
      }

      return(info)

    }, error = function(e) {
      # If there was an error extracting info from the ggplot object, just return
      # a list with the error message.
      return(structure(list(), error = e$message))
    })

  } else {
    print(p)
    return(list())
  }
}
