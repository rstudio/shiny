.globals$options <- list()

#' @param name Name of an option to get.
#' @param default Value to be returned if the option is not currently set.
#' @rdname shinyOptions
#' @export
getShinyOption <- function(name, default = NULL) {
  # Make sure to use named (not numeric) indexing
  name <- as.character(name)

  if (name %in% names(.globals$options))
    .globals$options[[name]]
  else
    default
}

#' Get or set Shiny options
#'
#' `getShinyOption` retrieves the value of a Shiny option.
#' `shinyOptions` sets the value of Shiny options; it can also be used to
#' return a list of all currently-set Shiny options.
#'
#' There is a global option set, which is available by default. When a Shiny
#' application is run with [runApp()], that option set is duplicated
#' and the new option set is available for getting or setting values. If options
#' are set from global.R, app.R, ui.R, or server.R, or if they are set from
#' inside the server function, then the options will be scoped to the
#' application. When the application exits, the new option set is discarded and
#' the global option set is restored.
#'
#' @section Options:
#'
#' There are a number of global options that affect Shiny's behavior. These can
#' be set with (for example) `options(shiny.trace=TRUE)`.
#'
#' \describe{
#'   \item{shiny.launch.browser}{A boolean which controls the default behavior
#'     when an app is run. See [runApp()] for more information.}
#'   \item{shiny.port}{A port number that Shiny will listen on. See
#'     [runApp()] for more information.}
#'   \item{shiny.trace}{Print messages sent between the R server and the web
#'     browser client to the R console. This is useful for debugging. Possible
#'     values are `"send"` (only print messages sent to the client),
#'     `"recv"` (only print messages received by the server), `TRUE`
#'     (print all messages), or `FALSE` (default; don't print any of these
#'     messages).}
#'   \item{shiny.autoreload}{If `TRUE` when a Shiny app is launched, the
#'     app directory will be continually monitored for changes to files that
#'     have the extensions: r, htm, html, js, css, png, jpg, jpeg, gif. If any
#'     changes are detected, all connected Shiny sessions are reloaded. This
#'     allows for fast feedback loops when tweaking Shiny UI.
#'
#'     Since monitoring for changes is expensive (we simply poll for last
#'     modified times), this feature is intended only for development.
#'
#'     You can customize the file patterns Shiny will monitor by setting the
#'     shiny.autoreload.pattern option. For example, to monitor only ui.R:
#'     `options(shiny.autoreload.pattern = glob2rx("ui.R"))`
#'
#'     The default polling interval is 500 milliseconds. You can change this
#'     by setting e.g. `options(shiny.autoreload.interval = 2000)` (every
#'     two seconds).}
#'   \item{shiny.reactlog}{If `TRUE`, enable logging of reactive events,
#'     which can be viewed later with the [reactlogShow()] function.
#'     This incurs a substantial performance penalty and should not be used in
#'     production.}
#'   \item{shiny.usecairo}{This is used to disable graphical rendering by the
#'     Cairo package, if it is installed. See [plotPNG()] for more
#'     information.}
#'   \item{shiny.maxRequestSize}{This is a number which specifies the maximum
#'     web request size, which serves as a size limit for file uploads. If
#'     unset, the maximum request size defaults to 5MB.}
#'   \item{shiny.suppressMissingContextError}{Normally, invoking a reactive
#'     outside of a reactive context (or [isolate()]) results in
#'     an error. If this is `TRUE`, don't error in these cases. This
#'     should only be used for debugging or demonstrations of reactivity at the
#'     console.}
#'   \item{shiny.host}{The IP address that Shiny should listen on. See
#'     [runApp()] for more information.}
#'   \item{shiny.json.digits}{The number of digits to use when converting
#'     numbers to JSON format to send to the client web browser.}
#'   \item{shiny.minified}{If this is `TRUE` or unset (the default), then
#'     Shiny will use minified JavaScript (`shiny.min.js`). If
#'     `FALSE`, then Shiny will use the un-minified JavaScript
#'     (`shiny.js`); this can be useful during development.}
#'   \item{shiny.error}{This can be a function which is called when an error
#'     occurs. For example, `options(shiny.error=recover)` will result a
#'     the debugger prompt when an error occurs.}
#'   \item{shiny.table.class}{CSS class names to use for tables.}
#'   \item{shiny.deprecation.messages}{This controls whether messages for
#'     deprecated functions in Shiny will be printed. See
#'     [shinyDeprecated()] for more information.}
#'   \item{shiny.fullstacktrace}{Controls whether "pretty" or full stack traces
#'     are dumped to the console when errors occur during Shiny app execution.
#'     The default is `FALSE` (pretty stack traces).}
#'   \item{shiny.stacktraceoffset}{If `TRUE`, then Shiny's printed stack
#'     traces will display srcrefs one line above their usual location. This is
#'     an arguably more intuitive arrangement for casual R users, as the name
#'     of a function appears next to the srcref where it is defined, rather than
#'     where it is currently being called from.}
#'   \item{shiny.sanitize.errors}{If `TRUE`, then normal errors (i.e.
#'     errors not wrapped in `safeError`) won't show up in the app; a simple
#'     generic error message is printed instead (the error and strack trace printed
#'     to the console remain unchanged). The default is `FALSE` (unsanitized
#'     errors).If you want to sanitize errors in general, but you DO want a
#'     particular error `e` to get displayed to the user, then set this option
#'     to `TRUE` and use `stop(safeError(e))` for errors you want the
#'     user to see.}
#'   \item{shiny.testmode}{If `TRUE`, then enable features for testing Shiny
#'     applications. If `FALSE` (the default), do not enable those features.
#'   }
#' }
#' @param ... Options to set, with the form `name = value`.
#' @aliases shiny-options
#' @examples
#' \dontrun{
#' shinyOptions(myOption = 10)
#' getShinyOption("myOption")
#' }
#' @export
shinyOptions <- function(...) {
  newOpts <- list(...)

  if (length(newOpts) > 0) {
    .globals$options <- dropNulls(mergeVectors(.globals$options, newOpts))
    invisible(.globals$options)
  } else {
    .globals$options
  }
}


# Eval an expression with a new option set
withLocalOptions <- function(expr) {
  oldOptionSet <- .globals$options
  on.exit(.globals$options <- oldOptionSet)

  expr
}


# Get specific shiny options and put them in a list, reset those shiny options,
# and then return the options list. This should be during the creation of a
# shiny app object, which happens before another option frame is added to the
# options stack (the new option frame is added when the app is run). This
# function "consumes" the options when the shinyApp object is created, so the
# options won't affect another app that is created later.
consumeAppOptions <- function() {
  options <- list(
    appDir = getwd(),
    bookmarkStore = getShinyOption("bookmarkStore")
  )

  shinyOptions(appDir = NULL, bookmarkStore = NULL)

  options
}

# Do the inverse of consumeAppOptions. This should be called once the app is
# started.
unconsumeAppOptions <- function(options) {
  if (!is.null(options)) {
    do.call(shinyOptions, options)
  }
}
