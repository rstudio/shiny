# Creates an object whose $ and $<- pass through to the parent
# session, unless the name is matched in ..., in which case
# that value is returned instead. (See Decorator pattern.)
createSessionProxy <- function(parentSession, ...) {
  e <- new.env(parent = emptyenv())
  e$parent <- parentSession
  e$overrides <- list(...)

  structure(
    e,
    class = "session_proxy"
  )
}

#' @export
`$.session_proxy` <- function(x, name) {
  if (name %in% names(x[["overrides"]]))
    x[["overrides"]][[name]]
  else
    x[["parent"]][[name]]
}

#' @export
`$<-.session_proxy` <- function(x, name, value) {
  x[["parent"]][[name]] <- value
  x
}

#' Invoke a Shiny module
#'
#' Shiny's module feature lets you break complicated UI and server logic into
#' smaller, self-contained pieces. Compared to large monolithic Shiny apps,
#' modules are easier to reuse and easier to reason about. See the article at
#' \url{http://shiny.rstudio.com/articles/modules.html} to learn more.
#'
#' @param module A Shiny module server function
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param ... Additional parameters to pass to module server function
#' @param session Session from which to make a child scope (the default should
#'   almost always be used)
#'
#' @return The return value, if any, from executing the module server function
#' @seealso \url{http://shiny.rstudio.com/articles/modules.html}
#'
#' @export
callModule <- function(module, id, ..., session = getDefaultReactiveDomain()) {
  childScope <- session$makeScope(id)

  withReactiveDomain(childScope, {
    if (!is.function(module)) {
      stop("module argument must be a function")
    }

    module(childScope$input, childScope$output, childScope, ...)
  })
}
