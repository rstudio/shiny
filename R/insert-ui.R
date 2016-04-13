#' Insert UI objects
#'
#' Insert a UI object into the web page.
#'
#' @export
insertUI <- function(selector,
  where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"),
  ui,
  immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(selector)
  force(ui)
  force(session)
  where <- match.arg(where)

  sendUI <- function() {
    session$sendCustomMessage("shiny-insert-ui",
      list(
        selector = selector,
        where = where,
        content = processDeps(ui, session)
      )
    )
  }

  if (!immediate) {
    session$onFlushed(sendUI, once = TRUE)
  } else {
    sendUI()
  }
}
