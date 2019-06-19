# Creates an object whose $ and [[ pass through to the parent
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
  if (name %in% names(.subset2(x, "overrides")))
    .subset2(x, "overrides")[[name]]
  else
    .subset2(x, "parent")[[name]]
}

#' @export
`[[.session_proxy` <- `$.session_proxy`


#' @export
`$<-.session_proxy` <- function(x, name, value) {
  # this line allows users to write into session$userData
  # (e.g. it allows something like `session$userData$x <- TRUE`,
  # but not `session$userData <- TRUE`) from within a module
  # without any hacks (see PR #1732)
  if (identical(x[[name]], value)) return(x)
  stop("Attempted to assign value on session proxy.")
}

`[[<-.session_proxy` <- `$<-.session_proxy`


#' Invoke a Shiny module
#'
#' Shiny's module feature lets you break complicated UI and server logic into
#' smaller, self-contained pieces. Compared to large monolithic Shiny apps,
#' modules are easier to reuse and easier to reason about. See the article at
#' <http://shiny.rstudio.com/articles/modules.html> to learn more.
#'
#' @param module A Shiny module server function
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param ... Additional parameters to pass to module server function
#' @param session Session from which to make a child scope (the default should
#'   almost always be used)
#'
#' @return The return value, if any, from executing the module server function
#' @seealso <http://shiny.rstudio.com/articles/modules.html>
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
