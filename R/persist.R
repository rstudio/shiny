#' Store a list of values
#'
#' @param values A named list of values to persist.
#'
#' @export
persistValues <- function(input, exclude = NULL, persist = FALSE,
  session = getDefaultReactiveDomain())
{
  id <- session$stateID

  tryCatch({
    stateDir <- file.path(persistentDir(), id)
    if (!dirExists(stateDir))
      dir.create(stateDir)

    # Serialize values, possibly saving some extra data to stateDir
    values <- serializeReactiveValues(input, exclude, stateDir)

    stateFile <- file.path(stateDir, "state.rds")
    saveRDS(values, stateFile)
  }, error = function(e) {
    stop(safeError(paste0("Unable to save state ", id)))
  })

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


  tryCatch({
    stateDir <- file.path(persistentDir(), id)
    stateFile <- file.path(stateDir, "state.rds")
    readRDS(stateFile)
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

