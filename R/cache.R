# A context object for tracking a cache that needs to be dirtied when a set of
# files changes on disk. Each time the cache is dirtied, the set of files is
# cleared. Therefore, the set of files needs to be re-built each time the cached
# code executes. This approach allows for dynamic dependency graphs.
CacheContext <- R6Class(
  'CacheContext',
  portable = FALSE,
  class = FALSE,
  public = list(
    .dirty = TRUE,
    # List of functions that return TRUE if dirty
    .tests = list(),

    addDependencyFile = function(file) {
      if (.dirty)
        return()

      file <- normalizePath(file)

      mtime <- file.info(file)$mtime
      .tests <<- c(.tests, function() {
        newMtime <- try(file.info(file)$mtime, silent=TRUE)
        if (inherits(newMtime, 'try-error'))
          return(TRUE)
        return(!identical(mtime, newMtime))
      })
      invisible()
    },
    forceDirty = function() {
      .dirty <<- TRUE
      .tests <<- list()
      invisible()
    },
    isDirty = function() {
      if (.dirty)
        return(TRUE)

      for (test in .tests) {
        if (test()) {
          forceDirty()
          return(TRUE)
        }
      }

      return(FALSE)
    },
    reset = function() {
      .dirty <<- FALSE
      .tests <<- list()
    },
    with = function(func) {
      oldCC <- .currentCacheContext$cc
      .currentCacheContext$cc <- self
      on.exit(.currentCacheContext$cc <- oldCC)

      return(func())
    }
  )
)

.currentCacheContext <- new.env()

# Indicates to Shiny that the given file path is part of the dependency graph
# for whatever is currently executing (so far, only ui.R). By default, ui.R only
# gets re-executed when it is detected to have changed; this function allows the
# caller to indicate that it should also re-execute if the given file changes.
#
# If NULL or NA is given as the argument, then ui.R will re-execute next time.
dependsOnFile <- function(filepath) {
  if (is.null(.currentCacheContext$cc))
    return()

  if (is.null(filepath) || is.na(filepath))
    .currentCacheContext$cc$forceDirty()
  else
    .currentCacheContext$cc$addDependencyFile(filepath)
}
