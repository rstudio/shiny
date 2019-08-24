#' @importFrom fastmap key_missing
#' @export
fastmap::key_missing

#' @importFrom fastmap is.key_missing
#' @export
fastmap::is.key_missing


validate_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || nchar(key) == 0) {
    stop("Invalid key: key must be single non-empty string.")
  }
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}

