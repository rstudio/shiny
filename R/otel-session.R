# Semantic conventions for session: https://opentelemetry.io/docs/specs/semconv/general/session/

#' Create and use session span and events
#'
#' If otel is disabled, the session span and events will not be created,
#' however the expression will still be evaluated.
#'
#' Span: `session_start`, `session_end`
#' @param expr Expression to evaluate within the session span
#' @param ... Ignored
#' @param domain The reactive domain
#' @noRd
use_session_start_ospan_async <- function(expr, ..., domain) {

  if (!has_otel_bind("session")) {
    return(force(expr))
  }

  id_attrs <- otel_session_id_attrs(domain)

  # Wrap the server initialization
  with_shiny_ospan_async(
    "session_start",
    expr,
    attributes = otel::as_attributes(c(
      id_attrs,
      otel_session_attrs(domain)
    ))
  )
}


with_session_end_ospan_async <- function(expr, ..., domain) {
  if (!has_otel_bind("session")) {
    return(force(expr))
  }

  id_attrs <- otel_session_id_attrs(domain)
  with_shiny_ospan_async(
    "session_end",
    expr,
    attributes = id_attrs
  )
}

# -- Helpers -------------------------------


# Occurs when the websocket connection is established
otel_session_attrs <- function(domain) {
  attrs <- list(
    PATH_INFO =
      sub(
        "/websocket/$", "/",
        domain[["request"]][["PATH_INFO"]] %||% ""
      ),
    HTTP_HOST = domain[["request"]][["HTTP_HOST"]] %||% "",
    HTTP_ORIGIN = domain[["request"]][["HTTP_ORIGIN"]] %||% "",
    ## Currently, Shiny does not expose QUERY_STRING when connecting the websocket
    # so we do not provide it here.
    # QUERY_STRING = domain[["request"]][["QUERY_STRING"]] %||% "",
    SERVER_PORT = domain[["request"]][["SERVER_PORT"]] %||% NA_integer_
  )
  # Safely convert SERVER_PORT to integer
  # If conversion fails, leave as-is (string or empty)
  # This avoids warnings/errors if SERVER_PORT is not a valid integer
  server_port <- suppressWarnings(as.integer(attrs$SERVER_PORT))
  if (!is.na(server_port)) {
    attrs$SERVER_PORT <- server_port
  }

  attrs
}

otel_session_id_attrs <- function(domain) {
  token <- domain$token
  if (is.null(token)) {
    return(list())
  }

  list(
    # Convention for client-side with session.start and session.end events
    # https://opentelemetry.io/docs/specs/semconv/general/session/
    #
    # Since we are the server, we'll add them as an attribute to _every_ span
    # within the session as we don't know exactly when they will be called.
    # Given it's only a single attribute, the cost should be minimal, but it ties every reactive calculation together.
    session.id = token
  )
}
