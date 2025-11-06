
# Used by otel to identify the tracer and logger for this package
# https://github.com/r-lib/otel/blob/afc31bc1f4bd177870d44b051ada1d9e4e685346/R/tracer-name.R#L33-L49
# DO NOT CHANGE THIS VALUE without understanding the implications for existing telemetry data!
otel_tracer_name <- "co.posit.r-package.shiny"

#' Create and use a Shiny OpenTelemetry span
#'
#' If otel is disabled, the span will not be created,
#' however the expression will still be evaluated.
#' @param name Span name
#' @param expr Expression to evaluate within the span
#' @param ... Ignored
#' @param attributes Optional span attributes
#' @return The result of evaluating `expr`
#' @noRd
with_otel_span <- function(name, expr, ..., attributes = NULL) {
  promises::with_otel_span(name, expr, ..., attributes = attributes, tracer = shiny_otel_tracer())
}


#' Start a Shiny OpenTelemetry span
#'
#' @param name Span name
#' @param ... Additional arguments passed to `otel::start_span()`
#' @return An OpenTelemetry span
#' @noRd
start_otel_span <- function(name, ...) {
  otel::start_span(name, ..., tracer = shiny_otel_tracer())
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_otel_span_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------


is_otel_span <- function(x) {
  inherits(x, "otel_span")
}

testthat__is_testing <- function() {
  # testthat::is_testing()
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Log a message using the Shiny OpenTelemetry logger
#'
#' @param msg The log message
#' @param ... Additional attributes to add to the log record
#' @param severity The log severity level (default: "info")
#' @param logger The OpenTelemetry logger to use (default: Shiny otel logger)
#' @return Invisibly returns.
#' @noRd
otel_log <- function(
  msg,
  ...,
  severity = "info",
  logger = shiny_otel_logger()
) {
  otel::log(msg, ..., severity = severity, logger = logger)
}

#' Check if OpenTelemetry tracing is enabled
#'
#' @param tracer The OpenTelemetry tracer to check (default: Shiny otel tracer)
#' @return `TRUE` if tracing is enabled, `FALSE` otherwise
#' @noRd
otel_is_tracing_enabled <- function(tracer = shiny_otel_tracer()) {
  otel::is_tracing_enabled(tracer)
}

#' Shiny OpenTelemetry logger
#'
#' Used for logging OpenTelemetry events via `otel_log()`
#' @return An OpenTelemetry logger
#' @noRd
shiny_otel_logger <- local({
  logger <- NULL

  # For internal testing purposes only
  reset_logger <- function() {
    logger <<- NULL
  }

  function() {
    if (!is.null(logger)) {
      return(logger)
    }

    this_logger <- otel::get_logger()

    if (testthat__is_testing()) {
      # Don't cache the logger in unit tests. It interferes with logger provider
      # injection in otelsdk::with_otel_record().
      return(this_logger)
    }
    logger <<- this_logger
    logger
  }
})



#' Shiny OpenTelemetry tracer
#'
#' Used for creating OpenTelemetry spans via `with_otel_span()` and
#' `start_otel_span()`
#'
#' Inspired by httr2:::get_tracer().
#' @return An OpenTelemetry tracer
#' @noRd
shiny_otel_tracer <- local({
  # Using local scope avoids an environment object lookup on each call.

  tracer <- NULL

  # For internal testing purposes only
  reset_tracer <- function() {
    tracer <<- NULL
  }

  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }

    this_tracer <- otel::get_tracer()

    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(this_tracer)
    }

    tracer <<- this_tracer
    tracer
  }
})
