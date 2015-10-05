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

#' @export
moduleUI <- function(module, id, ...) {
  if (is.function(module)) {
    # do nothing
  } else if (is.character(module)) {
    if (length(module) != 1) {
      stop("module should be a function or single-element character vector")
    }
    retval <- source(module, local = TRUE)$value
    if (!is.function(retval)) {
      stop("File ", module, " does not evaluate to a function")
    }
    module <- retval
  } else {
    stop("module should be a function or single-element character vector")
  }

  module(id, ...)
}

#' Invoke a module
#'
#' @param module A function or path
#' @param id An ID string
#' @param ... Additional parameters to pass to module function
#' @param session Session from which to make a child scope
#' @export
callModule <- function(module, id, ..., session = getDefaultReactiveDomain()) {
  childScope <- session$makeScope(id)

  withReactiveDomain(childScope, {
    if (is.function(module)) {
      # do nothing
    } else if (is.character(module)) {
      if (length(module) != 1) {
        stop("module should be a function or single-element character vector")
      }
      retval <- source(module, local = TRUE)$value
      if (!is.function(retval)) {
        stop("File ", module, " does not evaluate to a function")
      }
      module <- retval
    } else {
      stop("module should be a function or single-element character vector")
    }

    module(childScope$input, childScope$output, childScope, ...)
  })
}
