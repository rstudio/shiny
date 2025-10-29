OSPAN_REACTIVE_UPDATE_NAME <- "reactive_update"

# * `session$userData[["_otel_reactive_update_ospan"]]` - The active reactive update span (or `NULL`)
# * `session$userData[["_otel_has_reactive_cleanup"]]` - Whether the reactive span cleanup has been set

has_reactive_ospan_cleanup <- function(domain) {
  isTRUE(domain$userData[["_otel_has_reactive_cleanup"]])
}
set_reactive_ospan_cleanup <- function(domain) {
  domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE
}


reactive_update_ospan_is_active <- function(domain) {
  isTRUE(domain$userData[["_otel_reactive_update_is_active"]])
}
set_reactive_ospan_is_active <- function(domain) {
  domain$userData[["_otel_reactive_update_is_active"]] <- TRUE
}
clear_reactive_ospan_is_active <- function(domain) {
  domain$userData[["_otel_reactive_update_is_active"]] <- NULL
}

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

  if (!has_reactive_ospan_cleanup(domain)) {
    # Clean up any dangling reactive spans on an unplanned exit
    domain$onSessionEnded(function() {
      if (has_reactive_ospan_cleanup(domain)) {
        end_and_clear_reactive_update_ospan(domain = domain)
      }
    })
    set_reactive_ospan_cleanup(domain)
  }

  # Safety check
  prev_ospan <- domain$userData[["_otel_reactive_update_ospan"]]
  if (is_ospan(prev_ospan)) {
    stop("Reactive update span already exists")
  }

  reactive_update_ospan <- start_shiny_ospan(
    OSPAN_REACTIVE_UPDATE_NAME,
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
with_reactive_update_active_ospan <- function(expr, ..., domain) {
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
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
maybe_with_reactive_update_active_ospan <- function(expr, ..., domain) {
  if (!reactive_update_ospan_is_active(domain)) {
    set_reactive_ospan_is_active(domain)

    promises::hybrid_then(
      {
        with_reactive_update_active_ospan(domain = domain, expr)
      },
      on_success = function(value) {
        clear_reactive_ospan_is_active(domain)
      },
      on_failure = function(e) {
        clear_reactive_ospan_is_active(domain)
      },
      tee = TRUE
    )
  } else {
    expr
  }
}
