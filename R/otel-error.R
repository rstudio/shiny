
has_seen_otel_exception <- function(cnd) {
  !is.null(cnd$.shiny_otel_exception)
}

mark_otel_exception_as_seen <- function(cnd) {
  cnd$.shiny_otel_exception <- TRUE
  cnd
}

set_otel_exception_status_and_throw <- function(cnd) {
  cnd <- set_otel_exception_status(cnd)

  # Rethrow the (possibly updated) error
  signalCondition(cnd)
}

set_otel_exception_status <- function(cnd) {
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
    if (!has_seen_otel_exception(cnd)) {
      span$record_exception(
        # Record a sanitized error if sanitization is enabled
        get_otel_error_obj(cnd)
      )
      cnd <- mark_otel_exception_as_seen(cnd)
    }

    # Record the error status on the span for any context touching this error
    span$set_status("error")
  }

  cnd
}


get_otel_error_obj <- function(e) {
  # Do not expose errors to otel if sanitization is enabled
  if (getOption("shiny.otel.sanitize.errors", TRUE)) {
    sanitized_error()
  } else {
    e
  }
}
