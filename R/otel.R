#' @importFrom promises
#'   create_ospan
#'   end_ospan
#'   with_ospan_async
#'   with_ospan_promise_domain
#' @importFrom otel
#'   get_tracer
NULL

otel_tracer_name <- "co.posit.r-package.shiny"

with_shiny_ospan_async <- function(name, expr, ..., attributes = NULL) {
  with_ospan_async(name, expr, ..., attributes = attributes, tracer = get_tracer())
}

create_shiny_ospan <- function(name, ..., attributes = NULL) {
  create_ospan(name, ..., attributes = attributes, tracer = get_tracer())
}


# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------


otel_log_safe <- function(
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
