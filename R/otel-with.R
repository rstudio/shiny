#' Temporarily set OpenTelemetry (OTel) collection level
#'
#' @description
#' Control Shiny's OTel collection level for particular reactive expression(s).
#'
#' `withOtelCollect()` sets the OpenTelemetry collection level for
#' the duration of evaluating `expr`. `localOtelCollect()` sets the collection
#' level for the remainder of the current function scope.
#'
#' @details
#' Note that `"session"` and `"reactive_update"` levels are not permitted as
#' these are runtime-specific levels that should only be set permanently via
#' `options(shiny.otel.collect = ...)` or the `SHINY_OTEL_COLLECT` environment
#' variable, not temporarily during reactive expression creation.
#'
#' @section Best practice:
#'
#' Best practice is to set the collection level for code that *creates* reactive
#' expressions, not code that *runs* them. For instance:
#'
#' ```r
#' # Disable telemetry for a reactive expression
#' withOtelCollect("none", {
#'   my_reactive <- reactive({ ... })
#' })
#'
#' # Disable telemetry for a render function
#' withOtelCollect("none", {
#'   output$my_plot <- renderPlot({ ... })
#' })
#'
#' #' # Disable telemetry for an observer
#' withOtelCollect("none", {
#'   observe({ ... }))
#' })
#'
#' # Disable telemetry for an entire module
#' withOtelCollect("none", {
#'   my_result <- my_module("my_id")
#' })
#' # Use `my_result` as normal here
#' ```
#'
#' NOTE: It's not recommended to pipe existing reactive objects into
#' `withOtelCollect()` since they won't inherit their intended OTel settings,
#' leading to confusion.
#'
#' @param collect Character string specifying the OpenTelemetry collection level.
#'   Must be one of the following:
#'
#'     * `"none"` - No telemetry data collected
#'     * `"reactivity"` - Collect reactive execution spans (includes session and
#'       reactive update events)
#'     * `"all"` - All available telemetry (currently equivalent to `"reactivity"`)
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
