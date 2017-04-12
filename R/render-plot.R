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
#'   passed to \code{\link[grDevices]{png}}. Note that this affects the resolution of PNG
#'   rendering in R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}.
#'   These can be used to set the width, height, background color, etc.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param execOnResize If \code{FALSE} (the default), then when a plot is
#'   resized, Shiny will \emph{replay} the plot drawing commands with
#'   \code{\link[grDevices]{replayPlot}()} instead of re-executing \code{expr}.
#'   This can result in faster plot redrawing, but there may be rare cases where
#'   it is undesirable. If you encounter problems when resizing a plot, you can
#'   have Shiny re-execute the code on resize by setting this to \code{TRUE}.
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to \code{\link{plotOutput}} when \code{renderPlot} is used in an
#'   interactive R Markdown document.
#' @export
renderPlot <- function(expr, width='auto', height='auto', res=72, ...,
                       env=parent.frame(), quoted=FALSE,
                       execOnResize=FALSE, outputArgs=list()
) {
  # This ..stacktraceon is matched by a ..stacktraceoff.. when plotFunc
  # is called
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)

  args <- list(...)

  if (is.reactive(width))
    widthWrapper <- width
  else if (is.function(width))
    widthWrapper <- reactive({ width() })
  else
    widthWrapper <- function() { width }

  if (is.reactive(height))
    heightWrapper <- height
  else if (is.function(height))
    heightWrapper <- reactive({ height() })
  else
    heightWrapper <- function() { height }

  getDims <- function() {
    width <- widthWrapper()
    height <- heightWrapper()

    # Note that these are reactive calls. A change to the width and height
    # will inherently cause a reactive plot to redraw (unless width and
    # height were explicitly specified).
    if (width == 'auto')
      width <- session$clientData[[paste0('output_', outputName, '_width')]]
    if (height == 'auto')
      height <- session$clientData[[paste0('output_', outputName, '_height')]]

    list(width = width, height = height)
  }

  # Vars to store session and output, so that they can be accessed from
  # the plotObj() reactive.
  session <- NULL
  outputName <- NULL

  # Calls drawPlot, invoking the user-provided `func` (which may or may not
  # return a promise). The idea is that the (cached) return value from this
  # reactive can be used for varying width/heights, as it includes the
  # displaylist, which is resolution independent.
  drawReactive <- reactive({
    # Don't invalidate when width/height changes.
    dims <- isolate(getDims())
    pixelratio <- session$clientData$pixelratio %OR% 1
    p1 <- drawPlot(name, session, func, dims$width, dims$height, pixelratio, res)
    p1 <- promise::catch(p1, function(reason) {
      # Non-isolating read. A common reason for errors in plotting is because
      # the dimensions are too small. By taking a dependency on width/height,
      # we can try again if the plot output element changes size.
      getDims()

      # Propagate the error
      stop(reason)
    })
  })

  # This function is the one that's returned from renderPlot(), and gets
  # wrapped in an observer when the output value is assigned.
  renderFunc <- function(shinysession, name, ...) {
    outputName <<- name
    session <<- shinysession

    p1 <- drawReactive()
    p1 <- promise::then(p1, function(result) {
      dims <- getDims()
      pixelratio <- session$clientData$pixelratio %OR% 1
      resizeSavedPlot(name, shinysession, result, dims$width, dims$height, pixelratio, res)
    })
    p1
  }

  # If renderPlot isn't going to adapt to the height of the div, then the
  # div needs to adapt to the height of renderPlot. By default, plotOutput
  # sets the height to 400px, so to make it adapt we need to override it
  # with NULL.
  outputFunc <- plotOutput
  if (!identical(height, 'auto')) formals(outputFunc)['height'] <- list(NULL)

  markRenderFunction(outputFunc, renderFunc, outputArgs = outputArgs)
}

resizeSavedPlot <- function(name, session, result, width, height, pixelratio, res) {
  if (result$img$width == width && result$img$height == height &&
      result$pixelratio == pixelratio && result$res == res) {
    return(result$img)
  }
  #result$plotResult
  #result$recordedPlot

  coordmap <- NULL
  outfile <- plotPNG(function() {
    grDevices::replayPlot(result$recordedPlot)
    coordmap <<- getCoordmap(result$plotResult, width, height, pixelratio, res)
  }, width = width, height = height, res = res)
  on.exit(unlink(outfile), add = TRUE)

  img <- list(
    src = session$fileUrl(name, outfile, contentType = "image/png"),
    width = width,
    height = height,
    coordmap = coordmap,
    error = attr(coordmap, "error", exact = TRUE)
  )
}

drawPlot <- function(name, session, func, width, height, pixelratio, res, ...) {
  #  1. Start PNG
  #  2. Enable displaylist recording
  #  3. Call user-defined func
  #  4. Print/save result, if visible
  #  5. Snapshot displaylist
  #  6. Form coordmap
  #  7. End PNG (in finally)
  #  8. Form img tag
  #  9. Return img, value, displaylist, coordmap
  # 10. On error, take width and height dependency

  outfile <- tempfile(fileext='.png') # If startPNG throws, this could leak. Shrug.
  device <- startPNG(outfile, width, height, res = res, ...)
  domain <- createGraphicsDevicePromiseDomain(device)
  grDevices::dev.control(displaylist = "enable")

  p1 <- promise::with_promise_domain(domain, {
    p2 <- promise::resolved(func())
    p2 <- promise::then(p2, function(value, visible) {
      if (visible) {
        # A modified version of print.ggplot which returns the built ggplot object
        # as well as the gtable grob. This overrides the ggplot::print.ggplot
        # method, but only within the context of renderPlot. The reason this needs
        # to be a (pseudo) S3 method is so that, if an object has a class in
        # addition to ggplot, and there's a print method for that class, that we
        # won't override that method. https://github.com/rstudio/shiny/issues/841
        print.ggplot <- custom_print.ggplot

        # Use capture.output to squelch printing to the actual console; we
        # are only interested in plot output
        utils::capture.output({
          # This ..stacktraceon.. negates the ..stacktraceoff.. that wraps
          # the call to plotFunc. The value needs to be printed just in case
          # it's an object that requires printing to generate plot output,
          # similar to ggplot2. But for base graphics, it would already have
          # been rendered when func was called above, and the print should
          # have no effect.
          result <- ..stacktraceon..(print(value))
          # TODO jcheng 2017-04-11: Verify above ..stacktraceon..
        })
        result
      } else {
        # Not necessary, but I wanted to make it explicit
        NULL
      }
    })
    p2 <- promise::then(p2, function(value) {
      list(
        plotResult = value,
        recordedPlot = grDevices::recordPlot(),
        coordmap = getCoordmap(value, width, height, pixelratio, res),
        pixelratio = pixelratio,
        res = res
      )
    })
    p2
  })
  p1 <- promise::finally(p1, function() {
    grDevices::dev.off(device)
  })
  p1 <- promise::then(p1, function(result) {
    result$img <- dropNulls(list(
      src = session$fileUrl(name, outfile, contentType='image/png'),
      width = width,
      height = height,
      coordmap = result$coordmap,
      # Get coordmap error message if present
      error = attr(result$coordmap, "error", exact = TRUE)
    ))
    result
  })
  p1 <- promise::finally(p1, function() {
    unlink(outfile)
  })
}

# A modified version of print.ggplot which returns the built ggplot object
# as well as the gtable grob. This overrides the ggplot::print.ggplot
# method, but only within the context of renderPlot. The reason this needs
# to be a (pseudo) S3 method is so that, if an object has a class in
# addition to ggplot, and there's a print method for that class, that we
# won't override that method. https://github.com/rstudio/shiny/issues/841
custom_print.ggplot <- function(x) {
  grid::grid.newpage()

  build <- ggplot2::ggplot_build(x)

  gtable <- ggplot2::ggplot_gtable(build)
  grid::grid.draw(gtable)

  structure(list(
    build = build,
    gtable = gtable
  ), class = "ggplot_build_gtable")
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
# For ggplot2, first you need to define the print.ggplot function from inside
# renderPlot, then use it to print the plot:
# print.ggplot <- function(x) {
#   grid::grid.newpage()
#
#   build <- ggplot2::ggplot_build(x)
#
#   gtable <- ggplot2::ggplot_gtable(build)
#   grid::grid.draw(gtable)
#
#   structure(list(
#     build = build,
#     gtable = gtable
#   ), class = "ggplot_build_gtable")
# }
#
# p <- print(ggplot(mtcars, aes(wt, mpg)) + geom_point())
# str(getGgplotCoordmap(p, 1, 72))
# List of 1
#  $ :List of 10
#   ..$ panel     : int 1
#   ..$ row       : int 1
#   ..$ col       : int 1
#   ..$ panel_vars: Named list()
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
# p <- print(ggplot(mtc, aes(wt, mpg)) + geom_point() + facet_wrap(~ am))
# str(getGgplotCoordmap(p, 1, 72))
# List of 2
#  $ :List of 10
#   ..$ panel     : int 1
#   ..$ row       : int 1
#   ..$ col       : int 1
#   ..$ panel_vars:List of 1
#   .. ..$ panelvar1: Factor w/ 2 levels "0","1": 1
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


getCoordmap <- function(x, width, height, pixelratio, res) {
  if (inherits(x, "ggplot_build_gtable")) {
    getGgplotCoordmap(x, pixelratio, res)
  } else {
    getPrevPlotCoordmap(width, height)
  }
}

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

# Given a ggplot_build_gtable object, return a coordmap for it.
getGgplotCoordmap <- function(p, pixelratio, res) {
  if (!inherits(p, "ggplot_build_gtable"))
    return(NULL)

  tryCatch({
    # Get info from built ggplot object
    info <- find_panel_info(p$build)

    # Get ranges from gtable - it's possible for this to return more elements than
    # info, because it calculates positions even for panels that aren't present.
    # This can happen with facet_wrap.
    ranges <- find_panel_ranges(p$gtable, pixelratio, res)

    for (i in seq_along(info)) {
      info[[i]]$range <- ranges[[i]]
    }

    return(info)

  }, error = function(e) {
    # If there was an error extracting info from the ggplot object, just return
    # a list with the error message.
    return(structure(list(), error = e$message))
  })
}


find_panel_info <- function(b) {
  # Structure of ggplot objects changed after 2.1.0. After 2.2.1, there was a
  # an API for extracting the necessary information.
  ggplot_ver <- utils::packageVersion("ggplot2")

  if (ggplot_ver > "2.2.1") {
    find_panel_info_api(b)
  } else if (ggplot_ver > "2.1.0") {
    find_panel_info_non_api(b, ggplot_format = "new")
  } else {
    find_panel_info_non_api(b, ggplot_format = "old")
  }
}

# This is for ggplot2>2.2.1, after an API was introduced for extracting
# information about the plot object.
find_panel_info_api <- function(b) {
  # Workaround for check NOTE, until ggplot2 >2.2.1 is released
  colon_colon <- `::`
  # Given a built ggplot object, return x and y domains (data space coords) for
  # each panel.
  layout <- colon_colon("ggplot2", "summarise_layout")(b)
  coord  <- colon_colon("ggplot2", "summarise_coord")(b)
  layers <- colon_colon("ggplot2", "summarise_layers")(b)

  # Given x and y scale objects and a coord object, return a list that has
  # the bases of log transformations for x and y, or NULL if it's not a
  # log transform.
  get_log_bases <- function(xscale, yscale, coord) {
    # Given a transform object, find the log base; if the transform object is
    # NULL, or if it's not a log transform, return NA.
    get_log_base <- function(trans) {
      if (!is.null(trans) && grepl("^log-", trans$name)) {
        environment(trans$transform)$base
      } else {
        NA_real_
      }
    }

    # First look for log base in scale, then coord; otherwise NULL.
    list(
      x = get_log_base(xscale$trans) %OR% coord$xlog %OR% NULL,
      y = get_log_base(yscale$trans) %OR% coord$ylog %OR% NULL
    )
  }

  # Given x/y min/max, and the x/y scale objects, create a list that
  # represents the domain. Note that the x/y min/max should be taken from
  # the layout summary table, not the scale objects.
  get_domain <- function(xmin, xmax, ymin, ymax, xscale, yscale) {
    is_reverse <- function(scale) {
      identical(scale$trans$name, "reverse")
    }

    domain <- list(
      left   = xmin,
      right  = xmax,
      bottom = ymin,
      top    = ymax
    )

    if (is_reverse(xscale)) {
      domain$left  <- -domain$left
      domain$right <- -domain$right
    }
    if (is_reverse(yscale)) {
      domain$top    <- -domain$top
      domain$bottom <- -domain$bottom
    }

    domain
  }

  # Rename the items in vars to have names like panelvar1, panelvar2.
  rename_panel_vars <- function(vars) {
    for (i in seq_along(vars)) {
      names(vars)[i] <- paste0("panelvar", i)
    }
    vars
  }

  get_mappings <- function(layers, layout, coord) {
    # For simplicity, we'll just use the mapping from the first layer of the
    # ggplot object. The original uses quoted expressions; convert to
    # character.
    mapping <- layers$mapping[[1]]
    # lapply'ing as.character results in unexpected behavior for expressions
    # like `wt/2`; deparse handles it correctly.
    mapping <- lapply(mapping, deparse)

    # If either x or y is not present, give it a NULL entry.
    mapping <- mergeVectors(list(x = NULL, y = NULL), mapping)

    # The names (not values) of panel vars are the same across all panels,
    # so just look at the first one. Also, the order of panel vars needs
    # to be reversed.
    vars <- rev(layout$vars[[1]])
    for (i in seq_along(vars)) {
      mapping[[paste0("panelvar", i)]] <- names(vars)[i]
    }

    if (isTRUE(coord$flip)) {
      mapping[c("x", "y")] <- mapping[c("y", "x")]
    }

    mapping
  }

  # Mapping is constant across all panels, so get it here and reuse later.
  mapping <- get_mappings(layers, layout, coord)

  # If coord_flip is used, these need to be swapped
  flip_xy <- function(layout) {
    l <- layout
    l$xscale <- layout$yscale
    l$yscale <- layout$xscale
    l$xmin <- layout$ymin
    l$xmax <- layout$ymax
    l$ymin <- layout$xmin
    l$ymax <- layout$xmax
    l
  }
  if (coord$flip) {
    layout <- flip_xy(layout)
  }

  # Iterate over each row in the layout data frame
  lapply(seq_len(nrow(layout)), function(i) {
    # Slice out one row, use it as a list. The (former) list-cols are still
    # in lists, so we need to unwrap them.
    l <- as.list(layout[i, ])
    l$vars   <- l$vars[[1]]
    l$xscale <- l$xscale[[1]]
    l$yscale <- l$yscale[[1]]

    list(
      panel   = as.numeric(l$panel),
      row     = l$row,
      col     = l$col,
      # Rename panel vars. They must also be in reversed order.
      panel_vars = rename_panel_vars(rev(l$vars)),
      log     = get_log_bases(l$xscale, l$yscale, coord),
      domain  = get_domain(l$xmin, l$xmax, l$ymin, l$ymax, l$xscale, l$yscale),
      mapping = mapping
    )
  })
}


# This is for ggplot2<=2.2.1, before an API was introduced for extracting
# information about the plot object. The "old" format was used before 2.1.0.
# The "new" format was used after 2.1.0, up to 2.2.1. The reason these two
# formats are mixed together in a single function is historical, and it's not
# worthwhile to separate them at this point.
find_panel_info_non_api <- function(b, ggplot_format) {
  # Given a single range object (representing the data domain) from a built
  # ggplot object, return the domain.
  find_panel_domain <- function(b, panel_num, scalex_num = 1, scaley_num = 1) {
    if (ggplot_format == "new") {
      range <- b$layout$panel_ranges[[panel_num]]
    } else {
      range <- b$panel$ranges[[panel_num]]
    }
    domain <- list(
      left   = range$x.range[1],
      right  = range$x.range[2],
      bottom = range$y.range[1],
      top    = range$y.range[2]
    )

    # Check for reversed scales
    if (ggplot_format == "new") {
      xscale <- b$layout$panel_scales$x[[scalex_num]]
      yscale <- b$layout$panel_scales$y[[scaley_num]]
    } else {
      xscale <- b$panel$x_scales[[scalex_num]]
      yscale <- b$panel$y_scales[[scaley_num]]
    }
    if (!is.null(xscale$trans) && xscale$trans$name == "reverse") {
      domain$left  <- -domain$left
      domain$right <- -domain$right
    }
    if (!is.null(yscale$trans) && yscale$trans$name == "reverse") {
      domain$top    <- -domain$top
      domain$bottom <- -domain$bottom
    }

    domain
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
    if (ggplot_format == "new") {
      if (!is.null(b$layout$panel_scales$x[[scalex_num]]$trans))
        x_names <- b$layout$panel_scales$x[[scalex_num]]$trans$name
      if (!is.null(b$layout$panel_scales$y[[scaley_num]]$trans))
        y_names <- b$layout$panel_scales$y[[scaley_num]]$trans$name

    } else {
      if (!is.null(b$panel$x_scales[[scalex_num]]$trans))
        x_names <- b$panel$x_scales[[scalex_num]]$trans$name
      if (!is.null(b$panel$y_scales[[scaley_num]]$trans))
        y_names <- b$panel$y_scales[[scaley_num]]$trans$name
    }

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

    # Look for CoordFlip
    if (inherits(b$plot$coordinates, "CoordFlip")) {
      mappings[c("x", "y")] <- mappings[c("y", "x")]
    }

    mappings_cache <<- mappings
    mappings
  }

  if (ggplot_format == "new") {
    layout <- b$layout$panel_layout
  } else {
    layout <- b$panel$layout
  }
  # Convert factor to numbers
  layout$PANEL <- as.integer(as.character(layout$PANEL))

  # Names of facets
  facet_vars <- NULL
  if (ggplot_format == "new") {
    facet <- b$layout$facet
    if (inherits(facet, "FacetGrid")) {
      facet_vars <- vapply(c(facet$params$cols, facet$params$rows), as.character, character(1))
    } else if (inherits(facet, "FacetWrap")) {
      facet_vars <- vapply(facet$params$facets, as.character, character(1))
    }
  } else {
    facet <- b$plot$facet
    if (inherits(facet, "grid")) {
      facet_vars <- vapply(c(facet$cols, facet$rows), as.character, character(1))
    } else if (inherits(facet, "wrap")) {
      facet_vars <- vapply(facet$facets, as.character, character(1))
    }
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


# Given a gtable object, return the x and y ranges (in pixel dimensions)
find_panel_ranges <- function(g, pixelratio, res) {
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

  # Workaround for a bug in the quartz device. If you have a 400x400 image and
  # run `convertWidth(unit(1, "npc"), "native")`, the result will depend on
  # res setting of the device. If res=72, then it returns 400 (as expected),
  # but if, e.g., res=96, it will return 300, which is incorrect.
  devScaleFactor <- 1
  if (grepl("quartz", names(grDevices::dev.cur()), fixed = TRUE)) {
    devScaleFactor <- res / 72
  }

  # Convert a unit (or vector of units) to a numeric vector of pixel sizes
  h_px <- function(x) {
    devScaleFactor * grid::convertHeight(x, "native", valueOnly = TRUE)
  }
  w_px <- function(x) {
    devScaleFactor * grid::convertWidth(x, "native", valueOnly = TRUE)
  }

  # Given a vector of relative sizes (in grid units), and a function for
  # converting grid units to numeric pixels, return a list with: known pixel
  # dimensions, scalable dimensions, and the overall space for the scalable
  # objects.
  find_size_info <- function(rel_sizes, unit_to_px) {
    # Total pixels (in height or width)
    total_px <- unit_to_px(grid::unit(1, "npc"))
    # Calculate size of all panel(s) together. Panels (and only panels) have
    # null size.
    null_idx <- is_null_unit(rel_sizes)

    # All the absolute heights. At this point, null heights are 0. We need to
    # calculate them separately and add them in later.
    px_sizes <- unit_to_px(rel_sizes)
    # Mark the null heights as NA.
    px_sizes[null_idx] <- NA_real_

    # The plotting panels all are 'null' units.
    null_sizes <- rep(NA_real_, length(rel_sizes))
    null_sizes[null_idx] <- as.numeric(rel_sizes[null_idx])

    # Total size allocated for panels is the total image size minus absolute
    # (non-panel) elements.
    panel_px_total <- total_px - sum(px_sizes, na.rm = TRUE)

    # Size of a 1null unit
    null_px <- abs(panel_px_total / sum(null_sizes, na.rm = TRUE))

    # This returned list contains:
    # * px_sizes: A vector of known pixel dimensions. The values that were
    #   null units will be assigned NA. The null units are ones that scale
    #   when the plotting area is resized.
    # * null_sizes: A vector of the null units. All others will be assigned
    #   NA. The null units often are 1, but they may be any value, especially
    #   when using coord_fixed.
    # * null_px: The size (in pixels) of a 1null unit.
    # * null_px_scaled: The size (in pixels) of a 1null unit when scaled to
    #   fit a smaller dimension (used for plots with coord_fixed).
    list(
      px_sizes       = abs(px_sizes),
      null_sizes     = null_sizes,
      null_px        = null_px,
      null_px_scaled = null_px
    )
  }

  # Given a size_info, return absolute pixel positions
  size_info_to_px <- function(info) {
    px_sizes <- info$px_sizes

    null_idx <- !is.na(info$null_sizes)
    px_sizes[null_idx] <- info$null_sizes[null_idx] * info$null_px_scaled

    # If this direction is scaled down because of coord_fixed, we need to add an
    # offset so that the pixel locations are centered.
    offset <- (info$null_px - info$null_px_scaled) *
              sum(info$null_sizes, na.rm = TRUE) / 2

    # Get absolute pixel positions
    cumsum(px_sizes) + offset
  }

  heights_info <- find_size_info(g$heights, h_px)
  widths_info  <- find_size_info(g$widths,  w_px)

  if (g$respect) {
    # This is a plot with coord_fixed. The grid 'respect' option means to use
    # the same pixel value for 1null, for width and height. We want the
    # smaller of the two values -- that's what makes the plot fit in the
    # viewport.
    null_px_min <- min(heights_info$null_px, widths_info$null_px)
    heights_info$null_px_scaled <- null_px_min
    widths_info$null_px_scaled  <- null_px_min
  }

  # Convert to absolute pixel positions
  y_pos <- size_info_to_px(heights_info)
  x_pos <- size_info_to_px(widths_info)

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
