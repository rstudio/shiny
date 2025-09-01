#' Check if OpenTelemetry tracing is enabled
#' @noRd
is_otel_tracing <- function() {
  requireNamespace("otel", quietly = TRUE) && otel::is_tracing_enabled()
}


#' Create an OpenTelemetry span and return a closure to end it
#'
#' Creates an OpenTelemetry span for discontiguous operations where you need
#' manual control over when the span ends.
#'
#' Note, the created span is not activated. Please use [`with_ospan_async()`] to
#' activate it for the supplied expression.
#'
#' @param name Character string. The name of the span.
#' @param ...,attributes Additional arguments passed to `otel::start_span()`.
#'
#' @return An \pkg{otel} span object.
#'
#' @examples
#' \dontrun{
#' # Start a span for discontiguous work
#' my_operation_span <- create_ospan("my_operation")
#' # ... do some work ...
#' end_ospan(my_operation_span)
#' }
#'
#' @seealso [`with_ospan_async`] for continuous span operations
#' @noRd
create_ospan <- function(
  name,
  ...,
  attributes = NULL
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  span <- otel_create_span(
    name,
    ...,
    attributes = attributes
  )
  span
}

end_ospan <- function(span) {
  if (!is_otel_tracing()) {
    return(invisible())
  }

  otel::end_span(span)
}



#' Execute code within an OpenTelemetry span for asynchronous operations
#'
#' Creates an OpenTelemetry span and executes the given expression within it.
#' This function is designed to handle both synchronous and asynchronous
#' (promise-based) operations. For promises, the span is automatically ended
#' when the promise resolves or rejects.
#'
#' @param name Character string. The name of the span.
#' @param expr An expression to evaluate within the span context.
#' @param ...,attributes Additional arguments passed to `create_ospan()`.
#'
#' @return The result of evaluating `expr`. If `expr` returns a promise,
#'   the span will be automatically ended when the promise completes.
#'
#' @details
#' This function differs from synchronous span operations in that it installs
#' a promise domain and properly handles asynchronous operations. The span
#' will be ended either when the function exits (for synchronous operations)
#' or when a returned promise completes (for asynchronous operations).
#'
#' @examples
#' \dontrun{
#' # Synchronous operation
#' result <- with_ospan_async("my_operation", {
#'   # ... do some work ...
#'   42
#' })
#'
#' # Asynchronous operation
#' promise <- with_ospan_async("async_operation", {
#'   # ... return a promise ...
#'   some_async_function()
#' })
#' }
#'
#' @seealso [`create_ospan`] for manual span control,
#'   [`with_active_span_async`] for using already created spans
#' @noRd
with_ospan_async <- function(
  name,
  expr,
  ...,
  attributes = NULL
) {
  if (!is_otel_tracing()) {
    return(force(expr))
  }

  # message("\n\nStarting ospan: ", name)
  span <- create_ospan(name, ..., attributes = attributes)

  with_active_span_async(span, expr)
}

# Maybe move to {promises}?
with_active_span_async <- function(span, expr) {
  needs_cleanup <- TRUE
  cleanup <- function() {
    # message("\n\nEnding ospan: ", name)
    end_ospan(span)
  }
  on.exit(if (needs_cleanup) cleanup(), add = TRUE)

  with_ospan_promise_domain(span, {
    result <- force(expr)

    if (is.promising(result)) {
      needs_cleanup <- FALSE

      result <-
        promises::finally(result, cleanup)
    }

    result
  })
}



# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------

create_otel_active_span_promise_domain <- function(active_span) {
  force(active_span)
  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      function(...) {
        # print(names(active_span))
        otel::with_active_span(active_span, {

          # message(
          #   "Restoring active span: ",
          #   active_span$name,
          #   " - ",
          #   otel::get_active_span_context()$get_span_id()
          # )

          onFulfilled(...)
        })
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      function(...) {
        otel::with_active_span(active_span, {
          onRejected(...)
        })
      }
    },
    wrapSync = function(expr) {
      otel::with_active_span(active_span, {
        # message(
        #   "wrapSync Enter active span: ",
        #   active_span$name,
        #   " - ",
        #   otel::get_active_span_context()$get_span_id()
        # )
        # on.exit({
        #   message(
        #     "wrapSync Exit active span: ",
        #     active_span$name,
        #     " - ",
        #     otel::get_active_span_context()$get_span_id()
        #   )
        # }, add = TRUE)
        force(expr)
      })
    }
  )
}

with_ospan_promise_domain <- function(span, expr) {
  # # Reach into promises to pull the otel implementation out
  # with_otel_active_span_promise_domain <- rlang::ns_env("promises")[[
  #   "with_otel_active_span_promise_domain"
  # ]]

  act_span_pd <- create_otel_active_span_promise_domain(span)

  # Requires https://github.com/rstudio/promises/pull/166
  new_domain <- promises:::compose_domains(
    promises:::current_promise_domain(),
    act_span_pd
  )
  promises::with_promise_domain(new_domain, expr, replace = TRUE)
}

#' Create OpenTelemetry span
#'
#' Within the attributes, include the session token.
#' @noRd
otel_create_span <- function(
  name,
  ...,
  attributes = NULL
) {
  otel::start_span(
    name,
    ...,
    attributes = if (!is.null(attributes)) otel::as_attributes(attributes)
  )
}

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
