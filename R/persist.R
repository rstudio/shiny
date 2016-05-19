#' Store a list of values
#'
#' @param values A named list of values to persist.
#'
#' @return A unique ID of the persisted values.
#' @export
persistValues <- function(values, id, exclude = NULL) {
  # Serialize values, either to directory, or to a database.
  if (!is.list(values))
    stop("`values` must be a list.")
  if (anyUnnamed(values))
    stop("All values must be named.")

  persistFile <- file.path(persistentDir(), paste0(id, ".rds"))
  saveRDS(values, persistFile)
  id
}


#' Restore values
#'
#' @param id ID of a set of values to restore.
#'
#' @return A list of values that were persisted.
#' @export
restoreValues <- function(id) {
  if (is.null(id))
    stop("restoreValues requires an ID to restore.")

  persistFile <- file.path(persistentDir(), paste0(id, ".rds"))

  tryCatch({
    readRDS(persistFile)
  }, error = function(e) {
    stop(safeError(paste0("Unable to restore saved state ", id)))
  })
}



persistentDir <- function() {
  # This can be set by the hosting environment, like Shiny Server
  pdir <- getShinyOption('persistentDir', default = NULL)

  if (is.character(pdir)) {
    pdir
  } else if (is.function(pdir)) {
    pdir()
  } else if (is.null(pdir)) {
    # Try to persist in app's directory, or, if that's not available, in the
    # current directory.
    appdir <- getShinyOption("appDir", default = getwd())

    pdir <- file.path(appdir, "shiny_persist")
    if (!dirExists(pdir)) {
      dir.create(pdir)
    }

    pdir
  }
}

