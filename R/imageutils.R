startPNG <- function(filename, width, height, res, ..., device) {
  if (!is.function(device)) {
    stop("The `device` argument must be a function", call. = FALSE)
  }

  args <- rlang::list2(filename=filename, width=width, height=height, res=res, ...)

  if (is.null(args$bg)) {
    args$bg <- getCurrentOutputInfo()[["bg"]] %OR% "white"
  }

  # https://github.com/r-lib/ragg/issues/35
  fmls <- names(formals(device))
  if (("background" %in% fmls) && (!"bg" %in% fmls)) {
    if (is.null(args$background)) {
      args$background <- args$bg
    }
    args$bg <- NULL
  }

  do.call(device, args)
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

shinyDevice <- function() {
  if (capabilities("aqua")) {
    grDevices::png
  } else if ((getOption('shiny.usecairo') %OR% TRUE) &&
             nchar(system.file(package = "Cairo"))) {
    Cairo::CairoPNG
  } else {
    grDevices::png
  }
}


#' Run a plotting function and save the output as a PNG
#'
#' This function returns the name of the PNG file that it generates. In
#' essence, it calls `device()`, then `func()`, then `dev.off()`.
#' So `func` must be a function that will generate a plot when used this
#' way.
#'
#' The default `device` (i.e., `shinyDevice()`) is determined as follows:
#'  * If [quartz()] is operational, then [png()].
#'  * If installed and `options(shiny.usecairo=TRUE)` (the default), then [Cairo::CairoPNG()].
#'  * Otherwise, [png()].
#'
#' @param func A function that generates a plot.
#' @param filename The name of the output file. Defaults to a temp file with
#'   extension `.png`.
#' @param width Width in pixels.
#' @param height Height in pixels.
#' @param res Resolution in pixels per inch. This value is passed to
#'   `device`. Note that this affects the resolution of PNG rendering in
#'   R; it won't change the actual ppi of the browser.
#' @param ... Arguments to be passed through to `device`.
#'   These can be used to set the width, height, background color, etc.
#' @param device A function that opens a png graphics device (e.g., [png()]).
#' @export
plotPNG <- function(func, filename=tempfile(fileext='.png'),
                    width=400, height=400, res=72, ...,
                    device=getShinyOption("shiny.device", shinyDevice())) {
  dv <- startPNG(filename, width, height, res, ..., device = device)
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
