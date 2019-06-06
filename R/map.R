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

# Note that Map objects can't be saved in one R session and restored in
# another, because they are based on fastmap, which uses an external pointer,
# and external pointers can't be saved and restored in another session.
#' @importFrom fastmap fastmap
Map <- R6Class(
  'Map',
  portable = FALSE,
  public = list(
    initialize = function() {
      private$map <<- fastmap()
    },
    get = function(key) {
      map$get(key)
    },
    set = function(key, value) {
      map$set(key, value)
      value
    },
    mget = function(keys) {
      map$mget(keys)
    },
    mset = function(...) {
      map$mset(...)
    },
    remove = function(key) {
      if (!map$has(key))
        return(NULL)

      result <- map$get(key)
      map$remove(key)
      result
    },
    containsKey = function(key) {
      map$has(key)
    },
    keys = function(sort = FALSE) {
      map$keys(sort = sort)
    },
    values = function(sort = FALSE) {
      map$as_list(sort = sort)
    },
    clear = function() {
      map$reset()
    },
    size = function() {
      map$size()
    }
  ),

  private = list(
    map = NULL
  )
)

as.list.Map <- function(map) {
  map$values()
}
length.Map <- function(map) {
  map$size()
}
