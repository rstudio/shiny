startPNG <- function(filename, width, height, res, ...) {
  pngfun <- if ((getOption('shiny.useragg') %||% TRUE) && is_installed("ragg")) {
    ragg::agg_png
  } else if (capabilities("aqua")) {
    # i.e., png(type = 'quartz')
    grDevices::png
  } else if ((getOption('shiny.usecairo') %||% TRUE) && is_installed("Cairo")) {
    Cairo::CairoPNG
  } else {
    # i.e., png(type = 'cairo')
    grDevices::png
  }

  args <- list2(filename = filename, width = width, height = height, res = res, ...)

  # It's possible for width/height to be NULL/numeric(0) (e.g., when using
  # suspendWhenHidden=F w/ tabsetPanel(), see rstudio/shiny#1409), so when
  # this happens let the device determine what the default size should be.
  if (length(args$width) == 0) args$width <- NULL
  if (length(args$height) == 0) args$height <- NULL

  # Set a smarter default for the device's bg argument (based on thematic's global state).
  # Note that, technically, this is really only needed for CairoPNG, since the other
  # devices allow their bg arg to be overridden by par(bg=...), which thematic does prior
  # to plot-time, but it shouldn't hurt to inform other the device directly as well
  if (is.null(args$bg) && isNamespaceLoaded("thematic")) {
    args$bg <- getThematicOption("bg", "white")
    # auto vals aren't resolved until plot time, so if we see one, resolve it
    if (isTRUE("auto" == args$bg)) {
      args$bg <- getCurrentOutputInfo()[["bg"]]()
    }
  }

  # Handle both bg and background device arg
  # https://github.com/r-lib/ragg/issues/35
  fmls <- names(formals(pngfun))
  if (("background" %in% fmls) && (!"bg" %in% fmls)) {
    if (is.null(args$background)) {
      args$background <- args$bg
    }
    args$bg <- NULL
  }

  do.call(pngfun, args)
  # Call plot.new() so that even if no plotting operations are performed at
  # least we have a blank background. N.B. we need to set the margin to 0
  # temporarily before plot.new() because when the plot size is small (e.g.
  # 200x50), we will get an error "figure margin too large", which is triggered
  # by plot.new() with the default (large) margin. However, this does not
  # guarantee user's code in func() will not trigger the error -- they may have
  # to set par(mar = smaller_value) before they draw base graphics.
  op <- graphics::par(mar = rep(0, 4))
  tryCatch(
    graphics::plot.new(),
    finally = graphics::par(op)
  )

  grDevices::dev.cur()
}

#' Capture a plot as a PNG file.
#'
#' The PNG graphics device used is determined in the following order:
#'   * If the ragg package is installed (and the `shiny.useragg` is not
#'    set to `FALSE`), then use [ragg::agg_png()].
#'   * If a quartz device is available (i.e., `capabilities("aqua")` is
#'    `TRUE`), then use `png(type = "quartz")`.
#'   * If the Cairo package is installed (and the `shiny.usecairo` option
#'    is not set to `FALSE`), then use [Cairo::CairoPNG()].
#'   * Otherwise, use [grDevices::png()]. In this case, Linux and Windows
#'    may not antialias some point shapes, resulting in poor quality output.
#'
#' @details
#'   A `NULL` value provided to `width` or `height` is ignored (i.e., the
#'   default `width` or `height` of the graphics device is used).
#'
#' @param func A function that generates a plot.
#' @param filename The name of the output file. Defaults to a temp file with
#'   extension `.png`.
#' @param width Width in pixels.
#' @param height Height in pixels.
#' @param res Resolution in pixels per inch. This value is passed to the
#'   graphics device. Note that this affects the resolution of PNG rendering in
#'   R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to the graphics device. These can
#'   be used to set the width, height, background color, etc.
#'
#' @return A path to the newly generated PNG file.
#'
#' @export
plotPNG <- function(func, filename=tempfile(fileext='.png'),
                    width=400, height=400, res=72, ...) {
  dv <- startPNG(filename, width, height, res, ...)
  on.exit(grDevices::dev.off(dv), add = TRUE)
  func()

  filename
}

createGraphicsDevicePromiseDomain <- function(which = dev.cur()) {
  force(which)

  new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      function(...) {
        old <- dev.cur()
        dev.set(which)
        on.exit(dev.set(old))

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      function(...) {
        old <- dev.cur()
        dev.set(which)
        on.exit(dev.set(old))

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      old <- dev.cur()
      dev.set(which)
      on.exit(dev.set(old))

      force(expr)
    }
  )
}
