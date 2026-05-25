otel_collect_choices <- c(
  "none",
  "session",
  "reactive_update",
  "reactivity",
  "all"
)

# Check if the collect level is sufficient
otel_collect_is_enabled <- function(
  impl_level,
  # Listen to option and fall back to the env var
  opt_collect_level = getOption("shiny.otel.collect", Sys.getenv("SHINY_OTEL_COLLECT", "all"))
) {
  opt_collect_level <- as_otel_collect(opt_collect_level)

  which(opt_collect_level == otel_collect_choices) >=
    which(impl_level == otel_collect_choices)
}

# Check if tracing is enabled and if the collect level is sufficient
has_otel_collect <- function(collect) {
  # Only check pkg author input iff loaded with pkgload
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(collect) == 1, any(collect == otel_collect_choices))
  }

  otel_is_tracing_enabled() && otel_collect_is_enabled(collect)
}

# Run expr with otel collection disabled
with_no_otel_collect <- function(expr) {
  withOtelCollect("none", expr)
}


## -- Helpers -----------------------------------------------------

# shiny.otel.collect can be:
# "none"; To do nothing / fully opt-out
# "session" for session/start events
# "reactive_update" (includes "session" features) and reactive_update spans
# "reactivity" (includes "reactive_update" features) and spans for all reactive things
# "all" - Anything that Shiny can do. (Currently equivalent to the "reactivity" level)

as_otel_collect <- function(collect = "all") {
  if (!is.character(collect)) {
    stop("`collect` must be a character vector.")
  }

  # Match to collect enum
  collect <- match.arg(collect, otel_collect_choices, several.ok = FALSE)

  return(collect)
}
