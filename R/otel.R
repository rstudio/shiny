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


# Bench mark for
# * otel::get_tracer()
# * local scope get_tracer()
# * environment based get_tracer(), like httr2:::get_tracer()
# A tibble: 3 × 13
#   expression            min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result     memory     time
#   <bch:expr>        <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>     <list>     <list>
# 1 otel::get_tracer…  35.8µs  39.6µs    24238.      560B     12.1  9995     5   412.37ms <otl_trcr> <Rprofmem> <bench_tm>
# 2 get_tracer()        205ns   328ns  3044386.      560B      0   10000     0     3.29ms <otl_trcr> <Rprofmem> <bench_tm>
# 3 get_tracer_env()    328ns 451.1ns  2072708.      560B      0   10000     0     4.83ms <otl_trcr> <Rprofmem> <bench_tm>

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
