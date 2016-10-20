#' Update URL in browser's location bar
#'
#' This function updates the client browser's query string in the location bar.
#' It typically is called from an observer. Note that this will not work in
#' Internet Explorer 9 and below.
#'
#' @param state State to save on the history stack.
#' @param title Not currently in use.
#' @param url New url to display on the window.
#' @param session A Shiny session object.
#' @seealso \code{\link{enableBookmarking}} for examples.
#' @export
pushState <- function(state, title, url,
                              session = getDefaultReactiveDomain()) {
  session$pushState(state, title, url)
}
