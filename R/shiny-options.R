.globals$optionFrame <- new.env(parent = emptyenv())

#' @param name Name of an option to get.
#' @param default Value to be returned if the option is not currently set.
#' @rdname shinyOptions
#' @export
getShinyOption <- function(name, default = NULL) {
  if (exists(name, envir = .globals$optionFrame, inherits = TRUE))
    get(name, envir = .globals$optionFrame, inherits = TRUE)
  else
    default
}

#' @param ... Options to set, with the form \code{name = value}.
#' @rdname shinyOptions
#' @export
setShinyOption <- function(...) {
  list2env(list(...), .globals$optionFrame)
  invisible()
}

#' @param names A character vector of names to unset.
#' @rdname shinyOptions
#' @export
unsetShinyOption <- function(names) {
  # frame <- .globals$optionFrames[[length(.globals$optionFrames)]]
  #
  # Only try to remove names that actually exist in current frame.
  names <- intersect(names, ls(.globals$optionFrame, all.names = TRUE))
  rm(list = names, envir = .globals$optionFrame)
}

#' Get or set Shiny options
#'
#' \code{getShinyOption} retrieves the value of a Shiny option.
#' \code{setShinyOption} sets the value of a Shiny option. \code{shinyOptions}
#' returns a list of all currently-set Shiny options.
#'
#' Shiny stores its options in a stack of \emph{option frames}. There is a
#' global option frame, which is always available. When an app is run with
#' \code{\link{runApp}}, another option frame is pushed onto the stack.
#'
#' When \code{getShinyOption} or \code{shinyOptions} is called, they search the
#' current option frame and all others on the stack for a value, from the top
#' (most recent) down. If an option is set in more than one of these frames, the
#' value from the highest frame will be used.
#'
#' If options are set from global.R, app.R, ui.R, or server.R (outside of the
#' server function), they will be scoped to within the app.
#'
#' If an option is set to some value in a frame and then set to \code{NULL} in
#' that same frame, then when the value is fetched, it will be reported as
#' \code{NULL}. On the other hand, if an option is set to some value in a frame
#' and then \emph{un-set} with \code{unsetShinyOption}, then when the value is
#' fetched, it will be reported as the value from an option frame lower in the
#' option frame stack (if present).
#'
#' @examples
#' \dontrun{
#' setShinyOption(myOption = 10)
#' getShinyOption("myOption")
#' }
#' @export
shinyOptions <- function() {
  vals <- list()

  # Go down the stack of option frames, getting values. Make sure to preserve
  # the values from higher-level frames if any have the same name.
  frame <- .globals$optionFrame
  while (!identical(frame, emptyenv())) {
    vals <- mergeVectors(as.list(frame, all.names = TRUE), vals)
    frame <- parent.env(frame)
  }

  vals
}


pushNewOptionFrame <- function(frame) {
  oldFrame <- .globals$optionFrame
  .globals$optionFrame <- new.env(parent = .globals$optionFrame)
  oldFrame
}

restoreOptionFrame <- function(frame) {
  .globals$optionFrame <- frame
}

# Eval an expression with a new option frame on the stack
withNewOptionFrame <- function(expr) {
  oldFrame <- pushNewOptionFrame()
  on.exit(restoreOptionFrame(oldFrame))

  expr
}
