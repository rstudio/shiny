#' Process an HTML template
#'
#' @param filename Path to an HTML template file.
#' @param ... Variable values to use when processing the template.
#'
#' @export
htmlTemplate <- function(filename, ...) {
  html <- readChar(filename, 1e7)

  pieces <- strsplit(html, "{{", fixed = TRUE)[[1]]
  pieces <- strsplit(pieces, "}}", fixed = TRUE)

  # Each item in `pieces` is a 2-element character vector. In that vector, the
  # first item is code, and the second is text. The one exception is that the
  # first item in `pieces` will be a 1-element char vector; that element is
  # text.
  if (length(pieces[[1]]) != 1) {
    stop("Mismatched {{ and }} in HTML template.")
  }
  lapply(pieces[-1], function(x) {
    if (length(x) != 2) {
      stop("Mismatched {{ and }} in HTML template.")
    }
  })

  # Create environment to evaluate code. This environment gets the ... arguments
  # assigned as variables. It's a sibling of the global env, so that it doesn't
  # get variables or modify variables from the global env.
  env <- list2env(list(...), parent = parent.env(globalenv()))

  # For each item in `pieces` other than the first, run the code in the first subitem.
  pieces[-1] <- lapply(pieces[-1], function(piece) {
    piece[1] <- as.character(eval(parse(text = piece[1]), env))
    piece
  })

  HTML(paste(unlist(pieces, recursive = FALSE), collapse = ""))
}
