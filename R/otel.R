#' @importFrom promises
#'   with_ospan_async
#'   with_ospan_promise_domain
#'   local_ospan_promise_domain
NULL

otel_tracer_name <- "co.posit.r-package.shiny"

with_shiny_ospan_async <- function(name, expr, ..., attributes = NULL) {
  with_ospan_async(name, expr, ..., attributes = attributes, tracer = get_tracer())
}

create_shiny_ospan <- function(name, ...) {
  otel::start_span(name, ..., tracer = get_tracer())
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------


otel_log_safe <- function(
is_ospan <- function(x) {
  inherits(x, "otel_span")
}

testthat__is_testing <- function() {
  # testthat::is_testing()
  identical(Sys.getenv("TESTTHAT"), "true")
}
  msg,
  ...,
  severity = "info",
  logger = NULL
) {
  if (!otel::is_tracing_enabled()) return()
  # Use `"{msg}"` instead of `msg` to prevent otel from doing glue processing on the message
  # otel::log("{msg}", ..., severity = severity, logger = logger)
  # TODO: What happened to the processing?
  otel::log(msg, ..., severity = severity, logger = logger)
}

otel_is_tracing_enabled <- function(tracer = get_tracer()) {
  # bench marks: https://github.com/rstudio/promises/blob/a187fca5b24bd2235f15ccc0760de449c381d4ea/R/otel.R#L292-L299
  !.subset2(tracer, "is_enabled")()
}

get_ospan_logger <- local({
  logger <- NULL
  function() {
    if (!is.null(logger)) {
      return(logger)
    }
    if (testthat__is_testing()) {
      # Don't cache the logger in unit tests. It interferes with logger provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_logger())
    }
    logger <<- otel::get_logger()
    logger
  }
})



# Inspired by httr2:::get_tracer().
# Using local scope avoids an environment object lookup on each call.
get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})
