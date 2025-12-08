#' Temporarily set OpenTelemetry collection level
#'
#' @description
#' `withOtelCollect()` temporarily sets the OpenTelemetry collection level for
#' the duration of evaluating `expr`. `localOtelCollect()` sets the collection
#' level for the remainder of the current function scope.
#'
#' These functions are useful for temporarily controlling telemetry collection
#' during reactive expression creation. Only the following levels are allowed:
#' * `"none"` - No telemetry data collected
#' * `"reactivity"` - Collect reactive execution spans (includes session and
#'   reactive update events)
#' * `"all"` - All available telemetry (currently equivalent to `"reactivity"`)
#'
#' Note that `"session"` and `"reactive_update"` levels are not permitted as
#' these are runtime-specific levels that should only be set permanently via
#' `options(shiny.otel.collect = ...)` or the `SHINY_OTEL_COLLECT` environment
#' variable, not temporarily during reactive expression creation.
#'
#' @section Intended Usage:
#'
#' These functions are designed to perform sweeping changes to telemetry
#' collection, such as enabling or disabling OpenTelemetry for an entire module
#' or section of code where reactive expressions are being **created**:
#'
#' ```r
#' # Enable telemetry for an entire module
#' withOtelCollect("all", {
#'   my_result <- my_module("my_id")
#' })
#'
#' # Disable telemetry for expensive development-only reactives
#' withOtelCollect("none", {
#'   debug_reactive <- reactive({ expensive_debug_computation() })
#' })
#' ```
#'
#' @section Pipe Usage (Not Recommended):
#'
#' While these functions technically work with pipes, this usage is **not
#' recommended** because the reactive expression has already been created by the
#' time the pipe executes, so the telemetry settings have no effect:
#'
#' ```r
#' # Technically works, but not recommended
#' x <- reactive({ ... }) %>% withOtelCollect(collect = "all")
#' x <- reactive({ ... }) |> withOtelCollect(collect = "all")
#' # Equivalent to:
#' x <- withOtelCollect("all", reactive({ ... }))
#'
#' # Does NOT work as intended
#' x <- reactive({ ... })
#' # `x` was created outside of `withOtelCollect()`, so OTel settings are not applied
#' x_no_otel <- withOtelCollect("all", x)
#'
#' # Best practice is to create the reactive object inside the withOtelCollect() call
#' withOtelCollect("all", {
#'   x_with_otel <- reactive({ ... })
#'   y_with_otel <- reactive({ ... })
#' })
#' ```
#'
#' The correct approach is to create the reactive expression **inside** the
#' `withOtelCollect()` call or **after** setting the local collection level with
#' `localOtelCollect()`.
#'
#' @param collect Character string specifying the OpenTelemetry collection level.
#'   Must be one of `"none"`, `"reactivity"`, or `"all"`.
#' @param expr Expression to evaluate with the specified collection level
#'   (for `withOtelCollect()`).
#' @param envir Environment where the collection level should be set
#'   (for `localOtelCollect()`). Defaults to the parent frame.
#'
#' @return
#' * `withOtelCollect()` returns the value of `expr`.
#' * `localOtelCollect()` is called for its side effect and returns the previous
#'   `collect` value invisibly.
#'
#' @seealso See the `shiny.otel.collect` option within [`shinyOptions`]. Setting
#' this value will globally control OpenTelemetry collection levels.
#'
#' @examples
#' \dontrun{
#' # Temporarily disable telemetry collection
#' withOtelCollect("none", {
#'   # Code here won't generate telemetry
#'   reactive({ input$x + 1 })
#' })
#'
#' # Collect reactivity telemetry but not other events
#' withOtelCollect("reactivity", {
#'   # Reactive execution will be traced
#'   observe({ print(input$x) })
#' })
#'
#' # Use local variant in a function
#' my_function <- function() {
#'   localOtelCollect("none")
#'   # Rest of function executes without telemetry
#'   reactive({ input$y * 2 })
#' }
#' }
#'
#' @rdname withOtelCollect
#' @export
withOtelCollect <- function(collect, expr) {
  collect <- as_otel_collect_with(collect)

  withr::with_options(
    list(shiny.otel.collect = collect),
    expr
  )
}

#' @rdname withOtelCollect
#' @export
localOtelCollect <- function(collect, envir = parent.frame()) {
  collect <- as_otel_collect_with(collect)

  old <- withr::local_options(
    list(shiny.otel.collect = collect),
    .local_envir = envir
  )

  invisible(old)
}

# Helper function to validate collect levels for with/local functions
# Only allows "none", "reactivity", and "all" - not "session" or "reactive_update"
as_otel_collect_with <- function(collect) {
  if (!is.character(collect)) {
    stop("`collect` must be a character vector.")
  }

  allowed_levels <- c("none", "reactivity", "all")
  collect <- match.arg(collect, allowed_levels, several.ok = FALSE)

  return(collect)
}
