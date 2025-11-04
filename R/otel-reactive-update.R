# * `session$userData[["_otel_span_reactive_update"]]` - The active reactive update span (or `NULL`)


#' Start a `reactive_update` OpenTelemetry span and store it
#'
#' Used when a reactive expression is updated
#' Will only start the span iff the otel tracing is enabled
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso `otel_span_reactive_update_teardown()`
#' @noRd
otel_span_reactive_update_init <- function(..., domain) {

  if (!has_otel_bind("reactive_update")) return()

  # Ensure cleanup is registered only once per session
  if (is.null(domain$userData[["_otel_has_reactive_cleanup"]])) {
    domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE

    # Clean up any dangling reactive spans on an unplanned exit
    domain$onSessionEnded(function() {
      otel_span_reactive_update_teardown(domain = domain)
    })
  }

  # Safety check
  prev_otel_span <- domain$userData[["_otel_span_reactive_update"]]
  if (is_otel_span(prev_otel_span)) {
    stop("Reactive update span already exists")
  }

  reactive_update_otel_span <- start_otel_span(
    "reactive_update",
    ...,
    attributes = otel_session_id_attrs(domain)
  )

  domain$userData[["_otel_span_reactive_update"]] <- reactive_update_otel_span

  invisible()
}

#' End a `reactive_update` OpenTelemetry span and remove it from the session
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso `otel_span_reactive_update_init()`
#' @noRd
otel_span_reactive_update_teardown <- function(..., domain) {
  reactive_update_otel_span <- domain$userData[["_otel_span_reactive_update"]]

  if (is_otel_span(reactive_update_otel_span)) {
    otel::end_span(reactive_update_otel_span)
    domain$userData[["_otel_span_reactive_update"]] <- NULL
  }

  invisible()
}


#' Run expr within a `reactive_update` OpenTelemetry span
#'
#' Used to wrap the execution of a reactive expression. Will only
#' require/activate the span iff the otel tracing is enabled
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
with_otel_span_reactive_update <- function(expr, ..., domain) {
  reactive_update_otel_span <- domain$userData[["_otel_span_reactive_update"]]

  if (!is_otel_span(reactive_update_otel_span)) {
    return(force(expr))
  }

  # Given the reactive span is started before and ended when exec count is 0,
  # we only need to wrap the expr in the span context
  otel::with_active_span(reactive_update_otel_span, {force(expr)})
}


#' Run expr within `reactive_update` otel span if not already active
#'
#' If the reactive update otel span is not already active, run the expression
#' within the reactive update otel span context. This ensures that nested calls
#' to reactive expressions do not attempt to re-enter the same span.
#'
#' This method is used within Context `run()` and running an Output's observer
#' implementation
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
maybe_with_otel_span_reactive_update <- function(expr, ..., domain) {
  if (is.null(domain$userData[["_otel_reactive_update_is_active"]])) {
    domain$userData[["_otel_reactive_update_is_active"]] <- TRUE

    # When the expression is done promising, clear the active flag
    hybrid_then(
      {
        with_otel_span_reactive_update(domain = domain, expr)
      },
      on_success = function(value) {
        domain$userData[["_otel_reactive_update_is_active"]] <- NULL
      },
      on_failure = function(e) {
        domain$userData[["_otel_reactive_update_is_active"]] <- NULL
      },
      # Return the value before the callbacks
      tee = TRUE
    )
  } else {
    expr
  }
}
