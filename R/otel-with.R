otel_bind_choices <- c(
  "none",
  "session",
  "reactive_update",
  "reactivity",
  "all"
)

otel_bind_is_enabled <- function(impl_level, opt_bind_level = getOption("shiny.otel.bind", "all")) {
  opt_bind_level <- as_otel_bind(opt_bind_level)

  # if (opt_bind_level == "all") {
  #   return(TRUE)
  # }
  # if (opt_bind_level == "none") {
  #   return(FALSE)
  # }

  which(opt_bind_level == otel_bind_choices) >=
    which(impl_level == otel_bind_choices)
}


has_otel_bind <- function(bind) {
  # Only check pkg author input iff loaded with pkgload
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(bind) == 1, any(bind == otel_bind_choices))
  }

  otel::is_tracing_enabled() && otel_bind_is_enabled(bind)
}


#' Set OpenTelemetry options for Shiny reactives
#'
#' @param expr The expression to run with OpenTelemetry spans enabled.
#' @param ... Future parameter expansion.
#' @param bind If `"all"` (default), then all reactive objects will be bound to Open Telemetry spans.
#'   If `"none"`, then only the reactive objects created within the expression
#'   will be bound to Open Telemetry spans.
#' @noRd
#' @examples
#' # TODO: Make examples!!
withOtel <- function(expr, ..., bind = "all") {
  rlang::check_dots_empty()

  withr::with_options(
    list(
      shiny.otel.bind = as_otel_bind(bind)
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


# otel_bind_choices <-
#   c(
#     "reactiveVal",
#     "reactiveValues",
#     "reactiveExpr",
#     "observe",
#     "output",
#     "reactive_update",
#     "session"
#   )

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

  if (!identical(bind, "all") && !identical(bind, "none")) {
    stop(
      "`bind=` must be `\"all\"` or `\"none\"`."
    )
  }

  has_all <- any("all" == bind)
  has_none <- any("none" == bind)

  if (has_all && has_none) {
    stop("`bind` can not contain both 'all' and 'none'.")
  }

  if (has_all) {
    return(otel_bind_choices)
  }

  if (has_none) {
    return("none")
  }

  match.arg(bind, otel_bind_choices, several.ok = TRUE)
}
