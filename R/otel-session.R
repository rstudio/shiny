# Semantic conventions for session: https://opentelemetry.io/docs/specs/semconv/general/session/

#' Create and use session span and events
#'
#' If otel is disabled, the session span and events will not be created,
#' however the expression will still be evaluated.
#'
#' Events: `session.start` (immediately) and `session.end` (onSessionEnded)
#' Span: `session_start`
#' @param expr Expression to evaluate within the session span
#' @param ... Ignored
#' @param domain The reactive domain
#' @noRd
use_session_start_ospan_async <- function(expr, ..., domain) {

  if (!has_otel_bind("session")) {
    return(force(expr))
  }

  with_ospan_async("session_start", {
    local({
      # session_start
      spn <- otel::get_active_span()
      spn$add_event("session.start", attributes = list(
        session.id = domain$token
      ))
    })

    domain$onSessionEnded(function() {
      # session_end ospan
      spn <- otel::get_active_span()
      # On close, add session.end event
      spn$add_event("session.end", attributes = list(
        session.id = domain$token
      ))
    })

    force(expr)
  })
}


with_session_stop_ospan_async <- function(expr, ..., domain) {
  if (!has_otel_bind("session")) {
    return(force(expr))
  }

  with_ospan_async(
    "session_stop",
    attributes = list(session.id = domain$token),
    expr
  )
}

# -- Helpers -------------------------------

# otel_session_tracer <- function(domain) {
#   if (is.null(domain$userData[["_otel_tracer"]])) {

#     domain$userData[["_otel_tracer"]] <-
#       getOption(
#         "shiny.otel.tracer",
#         # TODO: Maybe the name is the folder name, similar to shinyapps.io naming
#         # Maybe set from a function call somewhere?
#         otel::get_tracer("Shiny app")
#       )
#   }
#   domain$userData[["_otel_tracer"]]
# }

otel_session_attrs <- function(domain) {
  attrs <- list(
    PATH_INFO = domain[["request"]][["PATH_INFO"]] %||% "",
    HTTP_HOST = domain[["request"]][["HTTP_HOST"]] %||% "",
    HTTP_ORIGIN = domain[["request"]][["HTTP_ORIGIN"]] %||% "",
    QUERY_STRING = domain[["request"]][["QUERY_STRING"]] %||% "",
    SERVER_PORT = domain[["request"]][["SERVER_PORT"]] %||% ""
  )
  try({
    attrs[["SERVER_PORT"]] <- as.integer(attrs[["SERVER_PORT"]])
  })
  attrs
}
