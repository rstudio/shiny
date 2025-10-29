# * `session$userData[["_otel_reactive_update_ospan"]]` - The active reactive update span (or `NULL`)


#' Start a `reactive_update` OpenTelemetry span and store it
#'
#' Used when a reactive expression is updated
#' Will only start the span iff the otel tracing is enabled
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso `end_and_clear_reactive_update_ospan()`
#' @noRd
start_and_store_reactive_update_ospan <- function(..., domain) {
  if (!has_otel_bind("reactive_update")) return()

  # Ensure cleanup is registered only once per session
  if (is.null(domain$userData[["_otel_has_reactive_cleanup"]])) {
    domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE

    # Clean up any dangling reactive spans on an unplanned exit
    domain$onSessionEnded(function() {
      end_and_clear_reactive_update_ospan(domain = domain)
    })
  }

  # Safety check
  prev_ospan <- domain$userData[["_otel_reactive_update_ospan"]]
  if (is_ospan(prev_ospan)) {
    stop("Reactive update span already exists")
  }

  reactive_update_ospan <- start_shiny_ospan(
    "reactive_update",
    ...,
    attributes = otel_session_id_attrs(domain)
  )

  domain$userData[["_otel_reactive_update_ospan"]] <- reactive_update_ospan
  return(invisible())
}

#' End a `reactive_update` OpenTelemetry span and remove it from the session
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso `start_and_store_reactive_update_ospan()`
#' @noRd
end_and_clear_reactive_update_ospan <- function(..., domain) {

  reactive_update_ospan <- domain$userData[["_otel_reactive_update_ospan"]]
  if (is_ospan(reactive_update_ospan)) {
    otel::end_span(reactive_update_ospan)
    domain$userData[["_otel_reactive_update_ospan"]] <- NULL
  }
}


#' Run expr within a `reactive_update` OpenTelemetry span
#'
#' Used to wrap the execution of a reactive expression. Will only
#' require/activate the span iff the otel tracing is enabled
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
with_existing_reactive_update_ospan <- function(expr, ..., domain) {
  reactive_update_ospan <- domain$userData[["_otel_reactive_update_ospan"]]

  if (!is_ospan(reactive_update_ospan)) {
    return(force(expr))
  }

  # Given the reactive span is started before and ended when exec count is 0,
  # we only need to wrap the expr in the span context
  otel::with_active_span(reactive_update_ospan, {force(expr)})
}


#' Run expr within `reactive_update` ospan if not already active
#'
#' If the reactive update ospan is not already active, run the expression
#' within the reactive update ospan context. This ensures that nested calls
#' to reactive expressions do not attempt to re-enter the same span.
#'
#' This method is used within Context `run()` and running an Output's observer
#' implementation
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
maybe_with_existing_reactive_update_ospan <- function(expr, ..., domain) {
  if (is.null(domain$userData[["_otel_reactive_update_is_active"]])) {
    domain$userData[["_otel_reactive_update_is_active"]] <- TRUE

    # When the expression is done promising, clear the active flag
    hybrid_then(
      {
        with_existing_reactive_update_ospan(domain = domain, expr)
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
