# TESTS
# Simple set/get
# Simple remove
# Simple containsKey
# Simple keys
# Simple values
# Simple clear
# Get of unknown key returns NULL
# Remove of unknown key does nothing
# Setting a key twice always results in last-one-wins
# /TESTS
Map <- R6Class(
  'Map',
  portable = FALSE,
  public = list(
    initialize = function() {
      private$env <- new.env(parent=emptyenv())
    },
    get = function(key) {
      env[[key]]
    },
    set = function(key, value) {
      env[[key]] <- value
      value
    },
    mget = function(keys) {
      base::mget(keys, env)
    },
    mset = function(...) {
      args <- list(...)
      if (length(args) == 0)
        return()

      arg_names <- names(args)
      if (is.null(arg_names) || any(!nzchar(arg_names)))
        stop("All elements must be named")

      list2env(args, envir = env)
    },
    remove = function(key) {
      if (!self$containsKey(key))
        return(NULL)

      result <- env[[key]]
      rm(list=key, envir=env, inherits=FALSE)
      result
    },
    containsKey = function(key) {
      exists(key, envir=env, inherits=FALSE)
    },
    keys = function() {
      # Sadly, this is much faster than ls(), because it doesn't sort the keys.
      names(as.list(env, all.names=TRUE))
    },
    values = function() {
      as.list(env, all.names=TRUE)
    },
    clear = function() {
      private$env <- new.env(parent=emptyenv())
      invisible(NULL)
    },
    size = function() {
      length(env)
    }
  ),

  private = list(
    env = 'environment'
  )
)

as.list.Map <- function(map) {
  map$values()
}
length.Map <- function(map) {
  map$size()
}
