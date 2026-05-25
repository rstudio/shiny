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

#' @export
as.list.Map <- function(x, ...) {
  x$values()
}

#' @export
length.Map <- function(x) {
  x$size()
}
