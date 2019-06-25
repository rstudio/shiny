#' A Key Missing object
#'
#' A `key_missing` object represents a cache miss.
#'
#' @param x An object to test.
#'
#' @seealso [diskCache()], [memoryCache()].
#'
#' @export
key_missing <- function() {
  structure(list(), class = "key_missing")
}

#' @rdname key_missing
#' @export
is.key_missing <- function(x) {
  inherits(x, "key_missing")
}

#' @export
print.key_missing <- function(x, ...) {
  cat("<Key Missing>\n")
}


validate_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || nchar(key) == 0) {
    stop("Invalid key: key must be single non-empty string.")
  }
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}
