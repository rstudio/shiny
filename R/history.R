#' Update URL in browser's location bar
#'
#' This function updates the client browser's query string in the location bar.
#' It typically is called from an observer. Note that this will not work in
#' Internet Explorer 9 and below.
#'
#' @param queryString	The new query string to show in the location bar.
#' @param session A Shiny session object.
#' @seealso \code{\link{enableBookmarking}} for examples.
#' @export
navigateQueryString <- function(queryString, session = getDefaultReactiveDomain()) {
  session$pushState(state = NULL, title = NULL, url = queryString)
}

