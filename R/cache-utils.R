
validate_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || nchar(key) == 0) {
    stop("Invalid key: key must be single non-empty string.")
  }
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}

# For our purposes, cache objects must support these methods.
is_cache_object <- function(x) {
  # Use tryCatch in case the object does not support `$`.
  tryCatch(
    is.function(x$get) && is.function(x$set),
    error = function(e) FALSE
  )
}

# Given a cache object, or string "app" or "session", return appropriate cache
# object.
resolve_cache_object <- function(cache, session) {
  if (identical(cache, "app")) {
    cache <- getShinyOption("cache", default = NULL)

  } else if (identical(cache, "session")) {
    cache <- session$cache
  }

  if (is_cache_object(cache)) {
    return(cache)
  }

  stop('`cache` must either be "app", "session", or a cache object with methods, `$get`, and `$set`.')
}
