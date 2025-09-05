OSPAN_REACTIVE_UPDATE_NAME <- "reactive_update"

# * `session$userData[["_otel_reactive_update_ospan"]]` - The active reactive update span (or `NULL`)
# * `session$userData[["_otel_has_reactive_cleanup"]]` - Whether the reactive span cleanup has been set

has_reactive_ospan_cleanup <- function(domain) {
  !is.null(domain$userData[["_otel_has_reactive_cleanup"]])
}
set_reactive_ospan_cleanup <- function(domain) {
  domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE
}

#' Create a `reactive_update` OpenTelemetry span
#'
#' Used when a reactive expression is updated
#' Will only start the span iff the otel tracing is enabled
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso [`end_reactive_update_ospan()`]
#' @noRd
create_reactive_update_ospan <- function(..., domain) {
  if (!has_otel_bind("reactive_update")) return()

  if (!has_reactive_ospan_cleanup(domain)) {
    # Clean up any dangling reactive span
    domain$onSessionEnded(function() {
      if (has_reactive_ospan_cleanup(domain)) {
        end_reactive_update_ospan(domain = domain)
      }
    })
    set_reactive_ospan_cleanup(domain)
  }

  prev_ospan <- domain$userData[["_otel_reactive_update_ospan"]]
  if (!is.null(prev_ospan)) {
    stop("Reactive update span already exists")
  }

  reactive_update_ospan <- create_shiny_ospan(
    OSPAN_REACTIVE_UPDATE_NAME,
    ...,
    attributes = list(
      # Pairs with session.start and session.end events
      # https://opentelemetry.io/docs/specs/semconv/general/session/
      # Since this encapsulates all reactive calculations elements,
      # the session.id isn't needed to be present in all lower spans
      session.id = domain$token
    )
  )

  domain$userData[["_otel_reactive_update_ospan"]] <- reactive_update_ospan
  return(invisible())
}

#' End a `reactive_update` OpenTelemetry span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso [`create_reactive_update_ospan()`]
#' @noRd
end_reactive_update_ospan <- function(..., domain) {

  reactive_update_ospan <- domain$userData[["_otel_reactive_update_ospan"]]
  if (!is.null(reactive_update_ospan)) {
    end_ospan(reactive_update_ospan)
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
with_reactive_update_ospan_async <- function(expr, ..., domain) {
  reactive_update_ospan <- domain$userData[["_otel_reactive_update_ospan"]]

  if (is.null(reactive_update_ospan)) {
    return(force(expr))
  }

  # Given the reactive span is started before and ended when exec count is 0,
  # we only need to wrap the expr in the span context
  otel::with_active_span(reactive_update_ospan, {force(expr)})
}
