# - OpenTelemetry -----------------------------------
# * Integration locations:
#   * √ Server:
#     * Start reactive_update when reactive busy count > 0
#     * End reactive_update when reactive busy count == 0
#   * √ Reactives: val, values, expr, render fn, observe
#   * Combinations:
#     * √ debounce() / throttle()
#     * bindCache()
#     * √ bindEvent()
#     * X - bindProgress()
#   * Special functions:
#     * ExtendedTask()
#       * Extended task links to submission reactive
#       * Reactive update that gets result links to the extended task
#     * √ observeEvent()
#     * √ eventReactive()
#       * TODO: Not recording updates within the span!!
#  * Maybe enhance all `withReactiveDomain()` calls?
# * Global options:
#   * √ shiny.otel.bind:
#     * "all", "none" - all or nothing
#     * Current non-public options:
#       * "session" - Adds session start/end events
#       * "reactive_update" - Spans for any reactive update. (Includes `"session"` features).
#       * "reactivity" - Spans for all reactive things. (Includes `"reactive_update"` features).
# * Private methods:
#   * bind_otel_*() - Methods that binds the reactive object to OpenTelemetry spans
#     * Note: When adding otel to an object, prepend a class of `FOO.otel`. Then add a dispatch method for `bindOtel.FOO.otel()` that declares the object already has been bound.
#   * with_no_otel_bind(expr) - Will not bind any reactives created within `expr` to OpenTelemetry spans.

# - TODO: -----------------------------------
# * Span status for success/failure (render function and regular reactive exprs?)
# * Error handling is not an "exception" for fatal logs
# * Connect `user.id` to be their user name: https://opentelemetry.io/docs/specs/semconv/registry/attributes/user/
# * Tests with otel recording



# - Questions -----------------------------------
# Add error handling for every otel. Use withCallingHandlers similar to https://github.com/r-lib/mirai/pull/395/files#diff-9e809582679952a93b9f34755bb38207471945eb36cedb9e2aa755125449f531R214-R215

# TODO: Extended Tasks are linked from parent span. Maybe use an envvar for span context? It is allowed for child process can end much later than the parent process. Take inspiration from callr PR (copying of the span context to the child process).

# ------------------------------------------

# # Approach
# Use flags on the reactive object to indicate whether to record OpenTelemetry spans.
#
# Cadence:
# * `$.isRecordingOtel` - Whether to record OpenTelemetry spans for this reactive object
# * `$.otelLabel` - The label to use for the OpenTelemetry span
# * `$.otelAttrs` - Additional attributes to add to the OpenTelemetry span


#' Add OpenTelemetry for reactivity to an object
#'
#' @description
#'
#' `bindOtel()` adds OpenTelemetry for [reactive()] expressions and `render*`
#' functions (like [renderText()], [renderTable()], ...).
#'
#' Wrapper to creating an active reactive OpenTelemetry span that closes when
#' the reactive expression is done computing. Typically this is when the
#' reactive expression finishes (synchronous) or when the returned promise is
#' done computing (asynchronous).

#' @section Async with OpenTelemetry:
#'
#'   With a reactive expression, the key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises --- not regular R
#'   promises, but rather objects provided by the
#'   \href{https://rstudio.github.io/promises/}{\pkg{promises}}  package, which
#'   are similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.) You can also use [mirai::mirai()] or [future::future()]
#'   objects to run code in a separate process or even on a remote machine.
#'
#'   When reactive expressions are being calculated in parallel (by having
#'   another reactive promise compute in the main process), the currently active
#'   OpenTelemetry span will be dynamically swapped out according to the
#'   currently active reactive expression. This means that as long as a promise
#'   was `then()`ed or `catch()`ed with an active OpenTelemetry span, the span
#'   will be correctly propagated to the next step (and subsequently other
#'   steps) in the promise chain.
#'
#'   While the common case is for a reactive expression to be created
#'   synchronously, troubles arise when the reactive expression is created
#'   asynchronously. The span **must** be created before the reactive expression
#'   is executed, it **must** be active for the duration of the expression, and
#'   it **must** not be closed until the reactive expression is done executing.
#'   This is not easily achieved with a single function call, so we provide a
#'   way to create a reactive expression that is bound to an OpenTelemetry
#'   span.
#'
#' @param x The object to add caching to.
#' @param ... Future parameter expansion.
#' @seealso [bindCache()] and [bindEvent()] for other ways to bind to your reactives.
#' @noRd
NULL

bind_otel_reactive_val <- function(x) {

  impl <- attr(x, ".impl", exact = TRUE)
  # Set flag for otel logging when setting the value
  impl$.isRecordingOtel <- TRUE

  class(x) <- c("reactiveVal.otel", class(x))

  x
}

bind_otel_reactive_values <- function(x) {

  impl <- .subset2(x, "impl")
  # Set flag for otel logging when setting values
  impl$.isRecordingOtel <- TRUE

  class(x) <- c("reactivevalues.otel", class(x))

  x
}

bind_otel_reactive_expr <- function(x) {

  domain <- reactive_get_domain(x)

  impl <- attr(x, "observable", exact = TRUE)
  impl$.isRecordingOtel <- TRUE
  # Covers both reactive and reactive.event
  impl$.otelLabel <- ospan_label_reactive(x, domain = impl$.domain)

  class(x) <- c("reactiveExpr.otel", class(x))

  x
}

bind_otel_observe <- function(x) {
  x$.isRecordingOtel <- TRUE
  x$.otelLabel <- ospan_label_observer(x, domain = x$.domain)

  class(x) <- c("Observer.otel", class(x))
  invisible(x)
}



bind_otel_shiny_render_function <- function(x) {

  valueFunc <- x
  span_label <- NULL
  ospan_attrs <- attr(x, "otelAttrs")

  renderFunc <- function(...) {
    # Dynamically determine the span label given the current reactive domain
    if (is.null(span_label)) {
      span_label <<-
        ospan_label_render_function(x, domain = getDefaultReactiveDomain())
    }

    with_shiny_ospan_async(
      span_label,
      {
        valueFunc(...)
      },
      attributes = ospan_attrs
    )
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- c("shiny.render.function.otel", class(valueFunc))
  renderFunc
}


# ## If we ever expose a S3 function, I'd like to add this method.
# bindOtel.function <- function(x, ...) {
#   cli::cli_abort(paste0(
#     "Don't know how to add OpenTelemetry recording to a plain function. ",
#     "If this is a {.code render*()} function for Shiny, it may need to be updated. ",
#     "Please see {.help shiny::bindOtel} for more information."
#   ))
# }
