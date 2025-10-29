#' @importFrom promises
#'   hybrid_then
#'   with_ospan_async
#'   with_ospan_promise_domain
#'   local_ospan_promise_domain
NULL

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
with_shiny_ospan_async <- function(name, expr, ..., attributes = NULL) {
  with_ospan_async(name, expr, ..., attributes = attributes, tracer = shiny_otel_tracer())
}

#' Start a Shiny OpenTelemetry span
#'
#' @param name Span name
#' @param ... Additional arguments passed to `otel::start_span()`
#' @return An OpenTelemetry span
#' @noRd
start_shiny_ospan <- function(name, ...) {
  otel::start_span(name, ..., tracer = shiny_otel_tracer())
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------


is_ospan <- function(x) {
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
