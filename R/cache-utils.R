#' A Key Missing object
#'
#' A \code{key_missing} object represents a cache miss.
#'
#' @seealso \code{\link{diskCache}}, \code{\link{memoryCache}}.
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
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}
