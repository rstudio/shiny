
has_seen_ospan_error <- function(cnd) {
  isTRUE(cnd$.shiny_error_seen)
}

set_ospan_error_as_seen <- function(cnd) {
  cnd$.shiny_error_seen <- TRUE
  cnd
}

set_ospan_error_status_and_throw <- function(cnd) {
  cnd <- set_ospan_error_status(cnd)

  # Rethrow the (possibly updated) error
  signalCondition(cnd)
}

set_ospan_error_status <- function(cnd) {
  if (inherits(cnd, "shiny.custom.error")) {
    # No-op
  } else if (inherits(cnd, "shiny.output.cancel")) {
    # No-op
  } else if (inherits(cnd, "shiny.output.progress")) {
    # No-op
  } else if (cnd_inherits(cnd, "shiny.silent.error")) {
    # No-op
  } else {
    # Only when an unknown error occurs do we set the span status to error
    span <- otel::get_active_span()

    # Only record the exception once at the original point of failure,
    # not every reactive expression that it passes through
    if (!has_seen_ospan_error(cnd)) {
      span$record_exception(
        # Record a sanitized error if sanitization is enabled
        get_otel_error_obj(cnd)
      )
      cnd <- set_ospan_error_as_seen(cnd)
    }

    # Record the error status on the span for any context touching this error
    span$set_status("error")
  }

  cnd
}


get_otel_error_obj <- function(e) {
  # Do not expose errors to otel if sanitization is enabled
  if (getOption("shiny.otel.sanitize.errors", FALSE)) {
    sanitized_error()
  } else {
    e
  }
}
