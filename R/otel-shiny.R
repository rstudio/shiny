# Used by otel to identify the tracer and logger for this package
# https://github.com/r-lib/otel/blob/afc31bc1f4bd177870d44b051ada1d9e4e685346/R/tracer-name.R#L33-L49
# DO NOT CHANGE THIS VALUE without understanding the implications for existing telemetry data!
otel_tracer_name <- "co.posit.r-package.shiny"

init_otel <- function() {
  .globals$otel_tracer <- otel::get_tracer()
  .globals$otel_is_tracing_enabled <- otel::is_tracing_enabled(.globals$otel_tracer)

  .globals$otel_logger <- otel::get_logger()
  # .globals$otel_is_logging_enabled <- otel::is_logging_enabled()
}
on_load({init_otel()})

#' Run expr within a Shiny OpenTelemetry recording context
#'
#' Reset the OpenTelemetry tracer and logger for Shiny.
#' Used for testing purposes only.
#' @param expr Expression to evaluate within the recording context
#' @return The result of evaluating `otelsdk::with_otel_record(expr)` with freshly enabled Shiny otel tracer and logger
#' @noRd
with_shiny_otel_record <- function(expr) {
  # Only use within internal testthat tests
  stopifnot(testthat__is_testing())
  withr::defer({ init_otel() })

  otelsdk::with_otel_record({
    init_otel()

    force(expr)
  })
}

#' Check if OpenTelemetry tracing is enabled
#'
#' @param tracer The OpenTelemetry tracer to check (default: Shiny otel tracer)
#' @return `TRUE` if tracing is enabled, `FALSE` otherwise
#' @noRd
otel_is_tracing_enabled <- function() {
  .globals[["otel_is_tracing_enabled"]]
}

#' Shiny OpenTelemetry logger
#'
#' Used for logging OpenTelemetry events via `otel_log()`
#' @return An OpenTelemetry logger
#' @noRd
shiny_otel_logger <- function() {
  .globals[["otel_logger"]]
}



#' Shiny OpenTelemetry tracer
#'
#' Used for creating OpenTelemetry spans via `with_otel_span()` and
#' `start_otel_span()`
#'
#' Inspired by httr2:::get_tracer().
#' @return An OpenTelemetry tracer
#' @noRd
shiny_otel_tracer <- function() {
  .globals[["otel_tracer"]]
}




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
