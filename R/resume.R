#' @include utils.R
NULL

#' Resume sessions after an interruption
#' @export
enableResume <- function(reload = c("ask", "resume", "fresh"), appVersion = NULL,
                         maxSize = 50e6, maxNodeSize = 5e6, debounce = 1000) {
  resume_check_top_level("enableResume")
  settings <- list(
    enabled = TRUE,
    reload = match.arg(reload),
    appVersion = resume_check_version(appVersion),
    maxSize = resume_check_number(maxSize, "maxSize", "a single positive number of bytes", positive = TRUE),
    maxNodeSize = resume_check_number(maxNodeSize, "maxNodeSize", "a single positive number of bytes", positive = TRUE),
    debounce = resume_check_number(debounce, "debounce", "a single non-negative number of milliseconds", positive = FALSE)
  )
  shinyOptions(resume = settings)
  invisible()
}

#' @rdname enableResume
#' @export
disableResume <- function(x) {
  if (missing(x)) {
    resume_check_top_level("disableResume")
    shinyOptions(resume = c(list(enabled = FALSE), resume_defaults()))
    return(invisible())
  }
  UseMethod("disableResume")
}

#' @export
disableResume.default <- function(x) {
  rlang::abort(paste0(
    "disableResume() expects a reactiveVal, reactiveValues object, or observer, not an object of class ",
    paste(class(x), collapse = "/"), "."
  ))
}

resume_check_top_level <- function(fn) {
  # server.R is sourced when the first session starts, after that session
  # resolved its settings; a call there would reach later sessions only.
  if (isTRUE(.globals$sourcingServerR)) {
    rlang::abort(sprintf("Call %s() in global.R, not server.R: server.R runs only once the first session has started.", fn))
  }
  if (is.null(getDefaultReactiveDomain())) return(invisible())
  rlang::abort(switch(fn,
    enableResume = "Call enableResume() at the top level of the app (in app.R before shinyApp(), or in global.R), not inside a session.",
    disableResume = "Call disableResume() at the top level of the app to turn resume off, or pass it a reactive."
  ))
}

resume_check_number <- function(value, arg, what, positive) {
  ok <- is.numeric(value) && length(value) == 1 && !is.na(value) &&
    (if (positive) value > 0 else value >= 0)
  if (!ok) rlang::abort(sprintf("`%s` must be %s.", arg, what))
  as.numeric(value)
}

resume_check_version <- function(value) {
  if (is.null(value)) return(NULL)
  if ((is.character(value) || is.numeric(value)) && length(value) == 1 && !is.na(value)) {
    return(as.character(value))
  }
  rlang::abort("`appVersion` must be NULL or a single string or number.")
}

#' @export
disableResume.reactiveVal <- function(x) {
  snapshot_mark_skipped(attr(x, ".impl", exact = TRUE))
  invisible(x)
}

#' @export
disableResume.reactivevalues <- function(x) {
  snapshot_mark_skipped(.subset2(x, "impl"))
  invisible(x)
}

#' @export
disableResume.Observer <- function(x) {
  x$.resumeSkip <- TRUE
  invisible(x)
}

# A node no registry tracks is never recorded, so there is nothing to mark.
snapshot_mark_skipped <- function(node, reason = "disableResume()") {
  state <- node$.snapshot
  if (!is.null(state)) {
    state$skip <- TRUE
    state$skipReason <- reason
  }
  invisible()
}

# Sources Shiny creates for itself: never recorded, never counted, never
# reported (spec 3.1).
snapshot_mark_internal <- function(x) {
  impl <- if (is.reactivevalues(x)) .subset2(x, "impl") else attr(x, ".impl", exact = TRUE)
  snapshot_mark_skipped(impl, "internal")
  invisible(x)
}
