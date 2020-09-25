#' @include globals.R
NULL

# The current app state is a place to read and hang state for the
# currently-running application. This is useful for setting options that will
# last as long as the application is running.

.globals$appState <- NULL

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

clearCurrentAppState <- function() {
  .globals$appState <- NULL
}
