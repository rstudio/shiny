startPNG <- function(filename, width, height, res, ...) {
  # If quartz is available, use png() (which will default to quartz).
  # Otherwise, if the Cairo package is installed, use CairoPNG().
  # Finally, if neither quartz nor Cairo, use png().
  if (capabilities("aqua")) {
    pngfun <- grDevices::png
  } else if ((getOption('shiny.usecairo') %OR% TRUE) &&
      nchar(system.file(package = "Cairo"))) {
    pngfun <- Cairo::CairoPNG
  } else {
    pngfun <- grDevices::png
  }

  pngfun(filename=filename, width=width, height=height, res=res, ...)
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
#' essence, it calls \code{png()}, then \code{func()}, then \code{dev.off()}.
#' So \code{func} must be a function that will generate a plot when used this
#' way.
#'
#' For output, it will try to use the following devices, in this order:
#' quartz (via \code{\link[grDevices]{png}}), then \code{\link[Cairo]{CairoPNG}},
#' and finally \code{\link[grDevices]{png}}. This is in order of quality of
#' output. Notably, plain \code{png} output on Linux and Windows may not
#' antialias some point shapes, resulting in poor quality output.
#'
#' In some cases, \code{Cairo()} provides output that looks worse than
#' \code{png()}. To disable Cairo output for an app, use
#' \code{options(shiny.usecairo=FALSE)}.
#'
#' @param func A function that generates a plot.
#' @param filename The name of the output file. Defaults to a temp file with
#'   extension \code{.png}.
#' @param width Width in pixels.
#' @param height Height in pixels.
#' @param res Resolution in pixels per inch. This value is passed to
#'   \code{\link[grDevices]{png}}. Note that this affects the resolution of PNG rendering in
#'   R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}.
#'   These can be used to set the width, height, background color, etc.
#' @export
plotPNG <- function(func, filename=tempfile(fileext='.png'),
                    width=400, height=400, res=72, ...) {
  dv <- startPNG(filename, width, height, res, ...)
  on.exit(grDevices::dev.off(dv), add = TRUE)
  func()

  filename
}

plotPNGAsync <- function(func, filename=tempfile(fileext='.png'),
  width=400, height=400, res=72, ...) {

  dv <- startPNG(filename, width, height, res, ...)
  domain <- createGraphicsDevicePromiseDomain(dv)
  p1 <- promise::with_promise_domain(domain, {
    p2 <- promise::resolved(NULL)
    p2 <- promise::then(p2, function(value) {
      func()
    })
    p2 <- promise::then(p2, function(value) {
      filename
    })
    p2
  })
  p1 <- promise::finally(p1, function() {
    grDevices::dev.off(dv)
  })
  p1
}

createGraphicsDevicePromiseDomain <- function(which = dev.cur()) {
  force(which)

  promise::new_promise_domain(
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
    }
  )
}
