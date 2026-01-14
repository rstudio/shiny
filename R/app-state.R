#' @include globals.R
NULL

# The current app state is a place to read and hang state for the
# currently-running application. This is useful for setting options that will
# last as long as the application is running.

.globals$appState <- NULL

#' Check whether a Shiny application is running
#'
#' This function tests whether a Shiny application is currently running.
#'
#' @return `TRUE` if a Shiny application is currently running. Otherwise,
#'   `FALSE`.
#' @export
isRunning <- function() {
  !is.null(getCurrentAppState())
}

initCurrentAppState <- function(appobj) {
  if (!is.null(.globals$appState)) {
    stop("Can't initialize current app state when another is currently active.")
  }
  .globals$appState <- new.env(parent = emptyenv())
  .globals$appState$app <- appobj
  # Copy over global options
  .globals$appState$options <- .globals$options
}

getCurrentAppState <- function() {
  .globals$appState
}

getCurrentAppStateOptions <- function() {
  .globals$appState$options
}
setCurrentAppStateOptions <- function(options) {
  stopifnot(isRunning())
  .globals$appState$options <- options
}

clearCurrentAppState <- function() {
  .globals$appState <- NULL
}
