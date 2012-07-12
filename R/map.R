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
Map <- setRefClass(
  'Map',
  fields = list(
    .env = 'environment'
  ),
  methods = list(
    initialize = function() {
      .env <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      if (.self$containsKey(key))
        return(base::get(key, pos=.env, inherits=F))
      else
        return(NULL)
    },
    set = function(key, value) {
      assign(key, value, pos=.env, inherits=F)
      return(value)
    },
    remove = function(key) {
      if (.self$containsKey(key)) {
        result <- .self$get(key)
        rm(list = key, pos=.env, inherits=F)
        return(result)
      }
      return(NULL)
    },
    containsKey = function(key) {
      exists(key, where=.env, inherits=F)
    },
    keys = function() {
      ls(envir=.env, all.names=T)
    },
    values = function() {
      mget(.self$keys(), envir=.env, inherits=F)
    },
    clear = function() {
      .env <<- new.env(parent=emptyenv())
      invisible(NULL)
    },
    size = function() {
      length(.env)
    }
  )
)

`[.Map` <- function(map, name) {
  map$get(name)
}

`[<-.Map` <- function(map, name, value) {
  map$set(name, value)
  return(map)
}

as.list.Map <- function(map) {
  sapply(map$keys(),
         map$get,
         simplify=F)
}
length.Map <- function(map) {
  map$size()
}
