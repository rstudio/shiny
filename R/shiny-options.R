.globals$options <- list()

#' @param name Name of an option to get.
#' @param default Value to be returned if the option is not currently set.
#' @rdname shinyOptions
#' @export
getShinyOption <- function(name, default = NULL) {
  # Make sure to use named (not numeric) indexing
  name <- as.character(name)

  # Check if there's a current session
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    if (name %in% names(session$options)) {
      return(session$options[[name]])
    } else {
      return(default)
    }
  }

  # Check if there's a current app
  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    if (name %in% names(app_state$options)) {
      return(app_state$options[[name]])
    } else {
      return(default)
    }
  }

  # If we got here, look in global options
  if (name %in% names(.globals$options)) {
    return(.globals$options[[name]])
  } else {
    return(default)
  }
}

#' Get or set Shiny options
#'
#' @description
#'
#' There are two mechanisms for working with options for Shiny. One is the
#' [options()] function, which is part of base R, and the other is the
#' `shinyOptions()` function, which is in the Shiny package. The reason for
#' these two mechanisms is has to do with legacy code and scoping.
#'
#' The [options()] function sets options globally, for the duration of the R
#' process. The [getOption()] function retrieves the value of an option. All
#' shiny related options of this type are prefixed with `"shiny."`.
#'
#' The `shinyOptions()` function sets the value of a shiny option, but unlike
#' `options()`, it is not always global in scope; the options may be scoped
#' globally, to an application, or to a user session in an application,
#' depending on the context. The `getShinyOption()` function retrieves a value
#' of a shiny option. Currently, the options set via `shinyOptions` are for
#' internal use only.
#'
#' @section Options with `options()`:
#'
#' \describe{
#' \item{shiny.autoreload (defaults to `FALSE`)}{If `TRUE` when a Shiny app is launched, the
#'   app directory will be continually monitored for changes to files that
#'   have the extensions: r, htm, html, js, css, png, jpg, jpeg, gif. If any
#'   changes are detected, all connected Shiny sessions are reloaded. This
#'   allows for fast feedback loops when tweaking Shiny UI.
#'
#'   Since monitoring for changes is expensive (we simply poll for last
#'   modified times), this feature is intended only for development.
#'
#'   You can customize the file patterns Shiny will monitor by setting the
#'   shiny.autoreload.pattern option. For example, to monitor only ui.R:
#'   `options(shiny.autoreload.pattern = glob2rx("ui.R"))`
#'
#'   The default polling interval is 500 milliseconds. You can change this
#'   by setting e.g. `options(shiny.autoreload.interval = 2000)` (every
#'   two seconds).}
#' \item{shiny.deprecation.messages (defaults to `TRUE`)}{This controls whether messages for
#'   deprecated functions in Shiny will be printed. See
#'   [shinyDeprecated()] for more information.}
#' \item{shiny.error (defaults to `NULL`)}{This can be a function which is called when an error
#'   occurs. For example, `options(shiny.error=recover)` will result a
#'   the debugger prompt when an error occurs.}
#' \item{shiny.error.unhandled (defaults to `NULL`)}{A function that will be
#'   called when an unhandled error that will stop the app session occurs. This
#'   function should take the error condition object as its first argument.
#'   Note that this function will not stop the error or prevent the session
#'   from ending, but it will provide you with an opportunity to log the error
#'   or clean up resources before the session is closed.}
#' \item{shiny.fullstacktrace (defaults to `FALSE`)}{Controls whether "pretty" (`FALSE`) or full
#'   stack traces (`TRUE`) are dumped to the console when errors occur during Shiny app execution.
#'   Pretty stack traces attempt to only show user-supplied code, but this pruning can't always
#'   be done 100% correctly.}
#' \item{shiny.host (defaults to `"127.0.0.1"`)}{The IP address that Shiny should listen on. See
#'   [runApp()] for more information.}
#' \item{shiny.jquery.version (defaults to `3`)}{The major version of jQuery to use.
#'   Currently only values of `3` or `1` are supported. If `1`, then jQuery 1.12.4 is used. If `3`,
#'   then jQuery `r version_jquery` is used.}
#' \item{shiny.json.digits (defaults to `I(16)`)}{Max number of digits to use when converting
#'   numbers to JSON format to send to the client web browser. Use [I()] to specify significant digits.
#'   Use `NA` for max precision.}
#' \item{shiny.launch.browser (defaults to `interactive()`)}{A boolean which controls the default behavior
#'   when an app is run. See [runApp()] for more information.}
#' \item{shiny.mathjax.url (defaults to `"https://mathjax.rstudio.com/latest/MathJax.js"`)}{
#'   The URL that should be used to load MathJax, via [withMathJax()].}
#' \item{shiny.mathjax.config (defaults to `"config=TeX-AMS-MML_HTMLorMML"`)}{The querystring
#'   used to load MathJax, via [withMathJax()].}
#' \item{shiny.maxRequestSize (defaults to 5MB)}{This is a number which specifies the maximum
#'   web request size, which serves as a size limit for file uploads.}
#' \item{shiny.minified (defaults to `TRUE`)}{By default
#'   Whether or not to include Shiny's JavaScript as a minified (`shiny.min.js`)
#'   or un-minified (`shiny.js`) file. The un-minified version is larger,
#'   but can be helpful for development and debugging.}
#' \item{shiny.port (defaults to a random open port)}{A port number that Shiny will listen on. See
#'   [runApp()] for more information.}
#' \item{shiny.reactlog (defaults to `FALSE`)}{If `TRUE`, enable logging of reactive events,
#'   which can be viewed later with the [reactlogShow()] function.
#'   This incurs a substantial performance penalty and should not be used in
#'   production.}
#' \item{shiny.sanitize.errors (defaults to `FALSE`)}{If `TRUE`, then normal errors (i.e.
#'   errors not wrapped in `safeError`) won't show up in the app; a simple
#'   generic error message is printed instead (the error and stack trace printed
#'   to the console remain unchanged). If you want to sanitize errors in general, but you DO want a
#'   particular error `e` to get displayed to the user, then set this option
#'   to `TRUE` and use `stop(safeError(e))` for errors you want the
#'   user to see.}
#' \item{shiny.stacktraceoffset (defaults to `TRUE`)}{If `TRUE`, then Shiny's printed stack
#'   traces will display srcrefs one line above their usual location. This is
#'   an arguably more intuitive arrangement for casual R users, as the name
#'   of a function appears next to the srcref where it is defined, rather than
#'   where it is currently being called from.}
#' \item{shiny.suppressMissingContextError (defaults to `FALSE`)}{Normally, invoking a reactive
#'   outside of a reactive context (or [isolate()]) results in
#'   an error. If this is `TRUE`, don't error in these cases. This
#'   should only be used for debugging or demonstrations of reactivity at the
#'   console.}
#' \item{shiny.testmode (defaults to `FALSE`)}{If `TRUE`, then various features for testing Shiny
#'   applications are enabled.}
#' \item{shiny.snapshotsortc (defaults to `FALSE`)}{If `TRUE`, test snapshot keys
#'   for \pkg{shinytest} will be sorted consistently using the C locale.  Snapshots
#'   retrieved by \pkg{shinytest2} will always sort using the C locale.}
#' \item{shiny.trace (defaults to `FALSE`)}{Print messages sent between the R server and the web
#'   browser client to the R console. This is useful for debugging. Possible
#'   values are `"send"` (only print messages sent to the client),
#'   `"recv"` (only print messages received by the server), `TRUE`
#'   (print all messages), or `FALSE` (default; don't print any of these
#'   messages).}
#' \item{shiny.autoload.r (defaults to `TRUE`)}{If `TRUE`, then the R/
#'   of a shiny app will automatically be sourced.}
#' \item{shiny.useragg (defaults to `TRUE`)}{Set to `FALSE` to prevent PNG rendering via the
#'   ragg package. See [plotPNG()] for more information.}
#' \item{shiny.usecairo (defaults to `TRUE`)}{Set to `FALSE` to prevent PNG rendering via the
#'   Cairo package. See [plotPNG()] for more information.}
#' \item{shiny.devmode (defaults to `NULL`)}{Option to enable Shiny Developer Mode. When set,
#'   different default `getOption(key)` values will be returned. See [devmode()] for more details.}
### Not documenting as 'shiny.devmode.verbose' is for niche use only
# ' \item{shiny.devmode.verbose (defaults to `TRUE`)}{If `TRUE`, will display messages printed
# '   about which options are being set. See [devmode()] for more details. }
### (end not documenting 'shiny.devmode.verbose')
#' }
#'
#'
#' @section Scoping for `shinyOptions()`:
#'
#'   There are three levels of scoping for `shinyOptions()`: global,
#'   application, and session.
#'
#'   The global option set is available by default. Any calls to
#'   `shinyOptions()` and `getShinyOption()` outside of an app will access the
#'   global option set.
#'
#'   When a Shiny application is run with [runApp()], the global option set is
#'   duplicated and the new option set is available at the application level. If
#'   options are set from `global.R`, `app.R`, `ui.R`, or `server.R` (but
#'   outside of the server function), then the application-level options will be
#'   modified.
#'
#'   Each time a user session is started, the application-level option set is
#'   duplicated, for that session. If the options are set from inside the server
#'   function, then they will be scoped to the session.
#'
#' @section Options with `shinyOptions()`:
#'
#'   There are a number of global options that affect Shiny's behavior. These
#'   can be set globally with `options()` or locally (for a single app) with
#'   `shinyOptions()`.
#'
#'   \describe{ \item{cache}{A caching object that will be used by
#'   [renderCachedPlot()]. If not specified, a [cachem::cache_mem()] will be
#'   used.} }
#'
#' @param ... Options to set, with the form `name = value`.
#' @aliases shiny-options
#' @export
shinyOptions <- function(...) {
  newOpts <- list2(...)

  if (length(newOpts) > 0) {
    # If we're within a session, modify at the session level.
    session <- getDefaultReactiveDomain()
    if (!is.null(session)) {
      # Modify session-level-options
      session$options <- dropNulls(mergeVectors(session$options, newOpts))
      return(invisible(session$options))
    }

    # If not in a session, but we have a currently running app, modify options
    # at the app level.
    app_state <- getCurrentAppState()
    if (!is.null(app_state)) {
      # Modify app-level options
      app_state$options <- dropNulls(mergeVectors(app_state$options, newOpts))
      return(invisible(app_state$options))
    }

    # If no currently running app, modify global options and return them.
    .globals$options <- dropNulls(mergeVectors(.globals$options, newOpts))
    return(invisible(.globals$options))
  }

  # If not setting any options, just return current option set, visibly.

  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    return(session$options)
  }

  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    return(app_state$options)
  }

  return(.globals$options)
}


# Get specific shiny options and put them in a list, reset those shiny options,
# and then return the options list. This should be during the creation of a
# shiny app object. This function "consumes" the options when the shinyApp
# object is created, so the options won't affect another app that is created
# later.
#
# ==== Example ====
# shinyOptions(bookmarkStore = 1234)
# # This now returns 1234.
# getShinyOption("bookmarkStore")
#
# # Creating the app captures the bookmarkStore option and clears it.
# s <- shinyApp(
#   fluidPage(verbatimTextOutput("txt")),
#   function(input, output) {
#     output$txt <- renderText(getShinyOption("bookmarkStore"))
#   }
# )
#
# # This now returns NULL.
# getShinyOption("bookmarkStore")
#
# When running the app, the app will display "1234"
# runApp(s)
#
# # After quitting the app, this still returns NULL.
# getShinyOption("bookmarkStore")
# ==================
#
# If another app had been created after s was created, but before s was run,
# then it would capture the value of "bookmarkStore" at the time of creation.
captureAppOptions <- function() {
  options <- list(
    appDir = getwd(),
    bookmarkStore = getShinyOption("bookmarkStore")
  )

  shinyOptions(appDir = NULL, bookmarkStore = NULL)

  options
}

# Do the inverse of captureAppOptions. This should be called once the app is
# started.
applyCapturedAppOptions <- function(options) {
  if (!is.null(options)) {
    do.call(shinyOptions, options)
  }
}
