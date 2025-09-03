#' @importFrom promises
#'   is_otel_tracing
#'   create_ospan
#'   end_ospan
#'   with_ospan_async
#'   with_ospan_promise_domain

# Just in case we don't use `shiny_otel_tracer()`, hopefully it is found using this variable for `otel::default_tracer_name()`
otel_tracer_name <- "co.posit.r-package.shiny"


# Only executed if otel actually exists
shiny_otel_tracer <- local({
  tracer <- NULL
  function() {
    if (is.null(tracer)) {
      tracer <<- otel::get_tracer(otel_tracer_name)
    }
    tracer
  }
})

with_shiny_ospan_async <- function(name, expr, ..., attributes = NULL) {
  with_ospan_async(name, expr, ..., attributes = attributes, tracer = shiny_otel_tracer())
}

create_shiny_ospan <- function(name, ..., attributes = NULL) {
  create_ospan(name, ..., attributes = attributes, tracer = shiny_otel_tracer())
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
  if (!is_otel_tracing()) return()
  # Use `"{msg}"` instead of `msg` to prevent otel from doing glue processing on the message
  # otel::log("{msg}", ..., severity = severity, logger = logger)
  # TODO: What happened to the processing?
  otel::log(msg, ..., severity = severity, logger = logger)
}
