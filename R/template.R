#' Process an HTML template
#'
#' @param filename Path to an HTML template file.
#' @param ... Variable values to use when processing the template.
#' @param document_ Is this template a complete HTML document (\code{TRUE}), or
#'   a fragment of HTML that is to be inserted into an HTML document
#'   (\code{FALSE})? With \code{"auto"} (the default), auto-detect by searching
#'   for the string \code{"<HTML>"} within the template.
#'
#' @export
htmlTemplate <- function(filename, ..., document_ = "auto") {
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
  vars <- list(...)
  if ("shinyHead" %in% names(vars)) {
    stop("Can't use reserved argument name 'shinyHead'.")
  }
  vars$shinyHead <- function() HTML("<!-- SHINY_HEAD -->")
  env <- list2env(vars, parent = parent.env(globalenv()))

  pieces[[1]] <- HTML(pieces[[1]])
  # For each item in `pieces` other than the first, run the code in the first subitem.
  pieces[-1] <- lapply(pieces[-1], function(piece) {
    tagList(
      eval(parse(text = piece[1]), env),
      HTML(piece[[2]])
    )
  })

  result <- tagList(pieces)

  if (document_ == "auto"  ) {
    document_ = grepl("<HTML>", html, ignore.case = TRUE)
  }
  if (document_) {
    # The html.document class indicates that it's a complete document, and not
    # just a set of tags.
    class(result) <- c("html.document", class(result))
  }

  result
}
