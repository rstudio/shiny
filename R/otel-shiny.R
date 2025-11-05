
# Used by otel to identify the tracer and logger for this package
# https://otel.r-lib.org/reference/default_tracer_name.html#setting-the-tracer-name
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

otel_log <- function(
  msg,
  ...,
  severity = "info",
  logger = shiny_otel_logger()
) {
  otel::log(msg, ..., severity = severity, logger = logger)
}

otel_is_tracing_enabled <- function(tracer = shiny_otel_tracer()) {
  otel::is_tracing_enabled(tracer)
}
otel_get_logger <- function() {
  otel::get_logger()
}
otel_get_tracer <- function() {
  otel::get_tracer()
}

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

    this_logger <- otel_get_logger()

    if (testthat__is_testing()) {
      # Don't cache the logger in unit tests. It interferes with logger provider
      # injection in otelsdk::with_otel_record().
      return(this_logger)
    }
    logger <<- this_logger
    logger
  }
})



# Inspired by httr2:::get_tracer().
# Using local scope avoids an environment object lookup on each call.
shiny_otel_tracer <- local({
  tracer <- NULL

  # For internal testing purposes only
  reset_tracer <- function() {
    tracer <<- NULL
  }

  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }

    this_tracer <- otel_get_tracer()

    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(this_tracer)
    }

    tracer <<- this_tracer
    tracer
  }
})
