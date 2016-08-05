# Function wrappers for saving and restoring state to/from disk when running
# Shiny locally.
#
# These functions provide a directory to the callback function.
#
# @param id A session ID to save.
# @param callback A callback function that saves state to or restores state from
#   a directory. It must take one argument, \code{stateDir}, which is a
#   directory to which it writes/reads.

saveInterfaceLocal <- function(id, callback) {
  # Try to save in app directory
  appDir <- getShinyOption("appDir", default = getwd())

  stateDir <- file.path(appDir, "shiny_bookmarks", id)
  if (!dirExists(stateDir))
    dir.create(stateDir, recursive = TRUE)

  callback(stateDir)
}

loadInterfaceLocal <- function(id, callback) {
  # Try to load from app directory
  appDir <- getShinyOption("appDir", default = getwd())

  stateDir <- file.path(appDir, "shiny_bookmarks", id)
  callback(stateDir)
}
