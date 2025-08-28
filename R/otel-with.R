
has_otel_bind <- function(bind) {
  # Only check pkg author input iff loaded with pkgload
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(bind) == 1, any(bind == otel_bind_choices))
  }

  is_otel_tracing() &&
    any(bind %in% as_otel_bind(getOption("shiny.otel.bind", "none")))
}

#' Set OpenTelemetry options for Shiny reactives
#'
#' @param expr The expression to run with OpenTelemetry spans enabled.
#' @param ... Future parameter expansion.
#' @param bindAll If `TRUE`, then all reactive objects will be bound to Open Telemetry spans.
#'   If `FALSE`, then only the reactive objects created within the expression
#'   will be bound to Open Telemetry spans.
#'   Defaults to `TRUE`.
#' @export
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

otel_bind_choices <-
  c(
    "reactiveVal",
    "reactiveValues",
    "reactiveExpr",
    "observe",
    "output",
    "reactive-update",
    "session"
  )

as_otel_bind <- function(bind = "all") {
  if (!is.character(bind)) {
    stop("`bind` must be a character vector.")
  }

  # As we get more supported options, we can add more choices
  # Must update docs in `shiny-options.R` for `shiny.otel.bind`
  # '   * `"reactiveVal"`, `"reactiveValues"`, `"reactiveExpr"`, `"observe"`, `"output"` - corresponding reactive objects
  # '   * `"reactive-update"` - Creates an OpenTelemetry span for a reactive
  # '     update. This corresponds to when Shiny knows of work to be completed (but
  # '     not necessarily blocking).
  # '   * `"session"` - Creates an OpenTelemetry span for each of the app's
  # '     sessions. It is activated for each call to the app's `server()` function.
  # '     The span is closed when the session is closed.
  # '
  # '   Any combination of these values can be provided to automatically track
  # '   activity within Shiny. If both `"all"` and `"none"` are provided, an error
  # '   will be thrown. For simpler use cases, `options(shiny.otel.bind =
  # '   c("session", "reactive-update"))` will suffice as it will create
  # '   OpenTelemetry spans for each session and their reactive update duration.
  # '
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
