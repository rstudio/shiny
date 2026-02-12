#' @include globals.R
NULL

# The current app state is a place to read and hang state for the
# currently-running application. This is useful for setting options that will
# last as long as the application is running.
#
# Multiple apps can run concurrently (in non-blocking mode). Each app gets its
# own appState env, stored in .globals$appStates (keyed by token). The direct
# pointer .globals$currentAppState is set by the service loop before each
# iteration, so getCurrentAppState() is a single env read on the hot path.

.globals$appStates <- list()
.globals$currentAppState <- NULL
.globals$serviceLoopRunning <- FALSE

initCurrentAppState <- function(appobj) {
  appState <- new.env(parent = emptyenv())
  appState$token <- createUniqueId(8)
  appState$app <- appobj
  # Copy over global options
  appState$options <- .globals$options
  # Per-app callback registries
  appState$onStopCallbacks <- Callbacks$new()
  appState$onUnhandledErrorCallbacks <- Callbacks$new()
  .globals$appStates[[appState$token]] <- appState
  .globals$currentAppState <- appState
  appState
}

getCurrentAppState <- function() {
  .globals$currentAppState
}

clearCurrentAppState <- function(token) {
  .globals$appStates[[token]] <- NULL
  # Clear pointer if it matches the token being removed
  if (!is.null(.globals$currentAppState) &&
      identical(.globals$currentAppState$token, token)) {
    .globals$currentAppState <- NULL
  }
}

anyAppRunning <- function() {
  length(.globals$appStates) > 0
}
