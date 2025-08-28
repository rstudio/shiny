OSPAN_SESSION_NAME <- "session"
OSPAN_REACTIVE_UPDATE_NAME <- "Reactive update"

# ## 2025-08-Barret - Motivation for this file
# This file was created to reduce maintainer cognitive load by providing
# fixed/smaller interfaces for working with OpenTelemetry spans and when spread
# across multiple files. While the function definitions may be small, the
# constants are only used within this file

# -- Start / stop ------------------------

#' Create a `session` OpenTelemetry span
#'
#' Used just as a session domain is created and activated before the app's
#' `server()` is called. Will only start the span iff the otel tracing is
#' enabled.
#'
#' Note: There is no `end_session_ospan()` as
#' [`end_forgotten_domain_ospans_on_session_end()`] will automatically clean up
#' the dangling ospan
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @param stop_on_session_end If `TRUE`, then the span will be ended when the
#' session ends. Defaults to `FALSE`.
#' @return Invisibly returns.
#' @noRd
create_session_ospan <- function(..., domain, stop_on_session_end) {
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(list(...)) == 0)
    stopifnot(isTRUE(stop_on_session_end))
  }

  print(lobstr::cst())

  if (!has_otel_bind("session")) return()

  create_domain_ospan(
    OSPAN_SESSION_NAME,
    # tracer = otel_session_tracer(domain),
    attributes = otel_session_attrs(domain),
    domain = domain
  )

  return(invisible())
}


#' Create a `reactive-update` OpenTelemetry span
#' Used when a reactive expression is updated
#' Will only start the span iff the otel tracing is enabled
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso [`end_reactive_update_ospan()`]
#' @noRd
create_reactive_update_ospan <- function(..., domain) {
  if (!has_otel_bind("reactive-update")) return()

  create_domain_ospan(
    OSPAN_REACTIVE_UPDATE_NAME,
    # tracer = otel_session_tracer(domain),
    links = list(
      session = get_session_ospan(domain = domain)
    ),
    domain = domain
  )

  return(invisible())
}
#' End a `reactive-update` OpenTelemetry span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @return Invisibly returns.
#' @seealso [`create_reactive_update_ospan()`]
#' @noRd
end_reactive_update_ospan <- function(..., domain) {
  # Always try to end the span
  end_domain_ospan(OSPAN_REACTIVE_UPDATE_NAME, domain = domain)
}


# -- With ------------------------

#' Run expr within a `session` OpenTelemetry span
#'
#' Used to wrap the execution of the server() code. Will only require/activate
#' the span iff the otel tracing is enabled
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
with_session_ospan_async <- function(expr, ..., domain) {
  with_existing_ospan_async(OSPAN_SESSION_NAME, domain = domain, expr)
}

#' Run expr within a `reactive-update` OpenTelemetry span
#'
#' Used to wrap the execution of a reactive expression. Will only
#' require/activate the span iff the otel tracing is enabled
#' @param expr The expression to executed within the span
#' @param ... Ignored
#' @param domain The reactive domain to associate with the span
#' @noRd
with_reactive_update_ospan_async <- function(expr, ..., domain) {
  with_existing_ospan_async(OSPAN_REACTIVE_UPDATE_NAME, domain = domain, expr)
}

# -- Get ------------------------


get_session_ospan <- function(..., domain) {
  get_ospan_from_domain(OSPAN_SESSION_NAME, domain = domain)
}

# get_reactive_update_ospan <- function(..., domain) {
#   get_ospan_from_domain(OSPAN_REACTIVE_UPDATE_NAME, domain = domain)
# }

# -- Has ------------------------

has_session_ospan <- function(..., domain) {
  has_existing_ospan(OSPAN_SESSION_NAME, domain = domain)
}

has_reactive_update_ospan <- function(..., domain) {
  has_existing_ospan(OSPAN_REACTIVE_UPDATE_NAME, domain = domain)
}

# -- Convenience ----------------------

#' Run expr within a `reactive-update` or `session` OpenTelemetry span
#'
#' Workflow:
#' * If the reactive update ospan exists, run the expression as is.
#' * Else if the session ospan exists, run the expression within the session ospan
#' * Else if allowed to force the expression, run it as is
#' @param expr The expression to executed
#' @param ... Ignored
#' @param force_expr Whether to force the expression to run
#' @param domain The reactive domain to associate with the span
#' @noRd
with_reactive_update_or_session_ospan_async <- function(expr, ..., force_expr = FALSE, domain) {
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(list(...)) == 0)
    stopifnot(is.logical(force_expr), length(force_expr) == 1)
  }
  # TODO-Q: Should this logic be changed to `If active span is valid, then force(expr)` `Else run under session ospan``
  if (has_reactive_update_ospan(domain = domain)) {
    # Perform as is because we are within some reactive span
    force(expr)
  } else if (has_session_ospan(domain = domain)) {
    # Attach to session ospan
    with_session_ospan_async(expr, domain = domain)
  } else if (force_expr) {
    force(expr)
  }
}

#' Safety method to close all spans once the Session is done
#'
#' Ex: `session` span is not closed so that it is the LAST otel domain to live
#'
#' It is recommended that all other spans are manually closed. If any are
#' somehow forgotten, they will be closed here.
#' @param domain The reactive domain to clean up
#' @noRd
end_forgotten_domain_ospans_on_session_end <- function(domain) {
  # Only register once
  if (!is.null(domain$userData[["_otel_on_end"]])) {
    return()
  }
  domain$userData[["_otel_on_end"]] <- TRUE

  domain$onSessionEnded(function(...) {
    # Remove all spans from the session
    message("Ending dangling spans")

    # Perform in reverse order as older spans were added first
    for (name in rev(get_ospan_names_from_domain(domain = domain))) {
      message("Ending dangling span: ", name)
      end_domain_ospan(name, domain = domain)
    }
  })
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


#' Create a span who will be stored on the session object
#'
#' Known spans:
#' * `"session"` (OSPAN_SESSION_NAME)
#' * `"Reactive update"` (OSPAN_REACTIVE_UPDATE_NAME)
#' @param name The name of the span to create
#' @param ... Additional arguments passed to the span creation
#' @param attributes Optional attributes to attach to the span
#' @param domain The reactive domain to associate with the span
#' @noRd
create_domain_ospan <- function(
  name,
  ...,
  attributes = NULL,
  domain
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  # Make sure there is no existing span
  cur_span <- get_ospan_from_domain(name, domain = domain)
  if (!is.null(cur_span)) {
    stop("An OpenTelemetry span with the name '", name, "' is already active in the specified domain.")
  }

  span <- otel_create_span(
    name,
    ...,
    attributes = attributes,
    domain = domain
  )

  # Store span in session
  set_ospan_in_domain(name, span, domain = domain)

  # Add a failsafe to end the span
  end_forgotten_domain_ospans_on_session_end(domain)

  return(invisible())
}


#' End a span who has been stored on the session object
#'
#' @param name The name of the span to end
#' @param ... Ignored
#' @param domain The reactive domain to clean up
#' @noRd
end_domain_ospan <- function(name, ..., domain) {
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(list(...)) == 0)
  }
  if (!is_otel_tracing()) {
    return(invisible())
  }

  span <- get_ospan_from_domain(name, domain = domain)

  if (is.null(span)) {
    # Span does not exist. Return early
    return(invisible())
  }

  otel::end_span(span)

  # Remove span from session
  set_ospan_in_domain(name, NULL, domain = domain)

  return(invisible())
}
