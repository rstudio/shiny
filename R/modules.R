# Creates an object whose $ and $<- pass through to the parent
# session, unless the name is matched in ..., in which case
# that value is returned instead. (See Decorator pattern.)
createSessionProxy <- function(parentSession, ...) {
  e <- new.env(parent = emptyenv())
  e$parent = parentSession
  e$overrides = list(...)

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
