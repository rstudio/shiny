startPNG <- function(filename, width, height, res, ...) {
  # shiny.useragg is an experimental option that isn't officially supported or
  # documented. It's here in the off chance that someone really wants
  # to use ragg (say, instead of showtext, for custom font rendering).
  # In the next shiny release, this option will likely be superseded in
  # favor of a fully customizable graphics device option
  if ((getOption('shiny.useragg') %OR% FALSE) && is_available("ragg")) {
    pngfun <- ragg::agg_png
  } else if (capabilities("aqua")) {
    # i.e., png(type = 'quartz')
    pngfun <- grDevices::png
  } else if ((getOption('shiny.usecairo') %OR% TRUE) && is_available("Cairo")) {
    pngfun <- Cairo::CairoPNG
  } else {
    # i.e., png(type = 'cairo')
    pngfun <- grDevices::png
  }

  args <- rlang::list2(filename=filename, width=width, height=height, res=res, ...)

  # Set a smarter default for the device's bg argument (based on thematic's global state).
  # Note that, technically, this is really only needed for CairoPNG, since the other
  # devices allow their bg arg to be overridden by par(bg=...), which thematic does prior
  # to plot-time, but it shouldn't hurt to inform other the device directly as well
  if (is.null(args$bg) && isNamespaceLoaded("thematic")) {
    # TODO: use :: once thematic is on CRAN
    args$bg <- utils::getFromNamespace("thematic_get_option", "thematic")("bg", "white")
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

#' Run a plotting function and save the output as a PNG
#'
#' This function returns the name of the PNG file that it generates. In
#' essence, it calls `png()`, then `func()`, then `dev.off()`.
#' So `func` must be a function that will generate a plot when used this
#' way.
#'
#' For output, it will try to use the following devices, in this order:
#' quartz (via [grDevices::png()]), then [Cairo::CairoPNG()],
#' and finally [grDevices::png()]. This is in order of quality of
#' output. Notably, plain `png` output on Linux and Windows may not
#' antialias some point shapes, resulting in poor quality output.
#'
#' In some cases, `Cairo()` provides output that looks worse than
#' `png()`. To disable Cairo output for an app, use
#' `options(shiny.usecairo=FALSE)`.
#'
#' @param func A function that generates a plot.
#' @param filename The name of the output file. Defaults to a temp file with
#'   extension `.png`.
#' @param width Width in pixels.
#' @param height Height in pixels.
#' @param res Resolution in pixels per inch. This value is passed to
#'   [grDevices::png()]. Note that this affects the resolution of PNG rendering in
#'   R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to [grDevices::png()].
#'   These can be used to set the width, height, background color, etc.
#' @export
plotPNG <- function(func, filename=tempfile(fileext='.png'),
                    width=400, height=400, res=72, ...) {
  dv <- startPNG(filename, width, height, res, ...)
  on.exit(grDevices::dev.off(dv), add = TRUE)
  func()

  filename
}

#' @importFrom grDevices dev.set dev.cur
createGraphicsDevicePromiseDomain <- function(which = dev.cur()) {
  force(which)

  promises::new_promise_domain(
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
