otel_bind_choices <- c(
  "none",
  "session",
  "reactive_update",
  "reactivity",
  "all"
)

# Check if the bind level is sufficient
otel_bind_is_enabled <- function(
  impl_level,
  # Listen to option and fall back to the env var
  opt_bind_level = getOption("shiny.otel.bind", Sys.getenv("SHINY_OTEL_BIND", "all"))
) {
  opt_bind_level <- as_otel_bind(opt_bind_level)

  which(opt_bind_level == otel_bind_choices) >=
    which(impl_level == otel_bind_choices)
}

# Check if tracing is enabled and if the bind level is sufficient
has_otel_bind <- function(bind) {
  # Only check pkg author input iff loaded with pkgload
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(bind) == 1, any(bind == otel_bind_choices))
  }

  otel_is_tracing_enabled() && otel_bind_is_enabled(bind)
}


# with_otel_bind <- function(
#   expr,
#   ...,
#   # bind = getOption("shiny.otel.bind", "all")
#   bind
# ) {
#   rlang::check_dots_empty()
#   bind <- as_otel_bind(bind)
#   withr::with_options(
#     list(
#       shiny.otel.bind = bind
#     ),
#     expr
#   )
# }

# Run expr with otel binding disabled
without_otel_bind <- function(expr) {
  withr::with_options(
    list(
      shiny.otel.bind = "none"
    ),
    expr
  )
}


## -- Helpers -----------------------------------------------------

# shiny.otel.bind can be:
# "none"; To do nothing / fully opt-out
# "session" for session/start events
# "reactive_update" (includes "session" features) and reactive_update spans
# "reactivity" (includes "reactive_update" features) and spans for all reactive things
# "all" - Anything that Shiny can do. (Currently equivalent to the "reactivity" level)

as_otel_bind <- function(bind = "all") {
  if (!is.character(bind)) {
    stop("`bind` must be a character vector.")
  }

  # Only allow for `"all"` or `"none"` for now
  if (!identical(bind, "all") && !identical(bind, "none")) {
    stop(
      "`bind=` must be `\"all\"` or `\"none\"`."
    )
  }

  ## When the check above is removed, add docs for shiny.otel.bind in shiny-options.R
  # '   Possible options:
  # '   * `"none"` - Shorthand for disabling all supported OpenTelemetry spans and logs.
  # '   * `"session"` - Adds support for tracing user sessions.
  # '   * `"reactive_update"` - Includes `"session"`, and adds support for tracing reactive updates.
  # '   * `"reactivity"` - Includes `"reactive_update"`, and adds support for tracing every reactive operation.
  # '   * `"all"` - Shorthand for recording all supported OpenTelemetry spans and logs. Currently equivalent to `"reactivity"`.

  # Match to bind enum
  bind <- match.arg(bind, otel_bind_choices, several.ok = FALSE)

  return(bind)
}
