# Function wrappers for persisting or restoring state when running Shiny locally
#
# These functions provide a directory to the callback function.
#
# @param id A session ID to save.
# @param callback A callback function that saves state to or restores state from
#   a directory. It must take one argument, \code{stateDir}, which is a
#   directory to which it writes/reads.

persistInterfaceLocal <- function(id, callback) {
  # Try to save in app directory, or, if that's not available, in the current
  # directory.
  appDir <- getShinyOption("appDir", default = getwd())

  stateDir <- file.path(appDir, "shiny_bookmarks", id)
  if (!dirExists(stateDir))
    dir.create(stateDir, recursive = TRUE)

  callback(stateDir)
}

loadInterfaceLocal <- function(id, callback) {
  # Try to save in app directory, or, if that's not available, in the current
  # directory.
  appDir <- getShinyOption("appDir", default = getwd())

  stateDir <- file.path(appDir, "shiny_bookmarks", id)
  callback(stateDir)
}
