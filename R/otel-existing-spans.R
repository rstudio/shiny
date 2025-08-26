OSPAN_SESSION_NAME <- "session"
OSPAN_REACTIVE_UPDATE_NAME <- "Reactive update"

# global blocking - reactivity
# session blocking - promises?
# non-blocking - extended tasks

# -- Start / stop ------------------------

# Used just as a domain is created and before the server() is called
# Will only start the span iff the otel tracing is enabled
# There is no `end_session_ospan()` as `end_forgotten_domain_ospans_on_session_end()` will automatically clean up the dangling ospan
start_session_ospan <- function(..., domain, stop_on_session_end = FALSE) {
  stopifnot(isTRUE(stop_on_session_end))

  if (!has_otel_bind("session")) return()

  start_domain_ospan(
    OSPAN_SESSION_NAME,
    # tracer = otel_session_tracer(domain),
    attributes = otel_session_attrs(domain),
    domain = domain
  )
}


start_reactive_update_ospan <- function(..., domain) {
  if (!has_otel_bind("reactive-update")) return()

  start_domain_ospan(
    OSPAN_REACTIVE_UPDATE_NAME,
    # tracer = otel_session_tracer(domain),
    links = list(
      session = get_session_ospan(domain = domain)
    ),
    domain = domain
  )
}
end_reactive_update_ospan <- function(..., domain) {
  # Always try to end the span
  end_domain_ospan(OSPAN_REACTIVE_UPDATE_NAME, domain = domain)
}


# -- With ------------------------

# Used to wrap the execution of the server() code
# Will only require the span iff the otel tracing is enabled
with_session_ospan_async <- function(expr, ..., domain) {
  with_existing_ospan_async(OSPAN_SESSION_NAME, domain = domain, expr)
}

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

# Wrap the expression in a reactive lock ospan.
# If the reactive lock ospan doesn't exist, use the session ospan
# If neither exist, do NOT execute the expression unless `force_expr = TRUE`
with_reactive_update_or_session_ospan_async <- function(expr, ..., force_expr = FALSE, domain) {
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

# Safety method to close all spans once the session is done
# Ex: `session` span is not closed so that it is the LAST otel domain to live
#
# It is recommended that all other spans are manually closed
# If any are somehow forgotten, they will be closed here
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
