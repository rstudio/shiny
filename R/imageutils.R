startPNG <- function(filename, width, height, res, ...) {
  args <- rlang::list2(filename=filename, width=width, height=height, res=res, ...)

  if (is.null(args$bg)) {
    args$bg <- getCurrentOutputInfo()[["bg"]] %OR% "white"
  }

  # If quartz is available, use png() (which will default to quartz).
  # Otherwise, if the Cairo package is installed, use CairoPNG().
  # Finally, if neither quartz nor Cairo, use png().
  if (capabilities("aqua")) {
    pngfun <- grDevices::png
  } else if ((getOption('shiny.useragg') %OR% TRUE) &&
             nchar(system.file(package = "ragg"))) {
    pngfun <- ragg::agg_png
    args$background <- args$background %OR% args$bg
    args$bg <- NULL
  } else if ((getOption('shiny.usecairo') %OR% TRUE) &&
             nchar(system.file(package = "Cairo"))) {
    pngfun <- Cairo::CairoPNG
  } else {
    pngfun <- grDevices::png
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
