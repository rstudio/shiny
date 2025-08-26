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
#' @param ... Additional arguments passed to `otel::start_span()`.
#' @param domain The reactive domain to associate with the span. Defaults to
#'   the current reactive domain from `getDefaultReactiveDomain()`.
#'
#' @return An \pkg{otel} span object.
#'
#' @examples
#' \dontrun{
#' # Start a span for discontiguous work
#' my_operation_span <- create_ospan("my_operation", domain = domain)
#' # ... do some work ...
#' end_ospan(my_operation_span)
#' }
#'
#' @seealso [`with_ospan_async`] for continuous span operations
#' @keywords internal
create_ospan <- function(
  name,
  ...,
  attributes = NULL,
  domain
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  span <- otel_start_span(
    name,
    ...,
    attributes = attributes,
    domain = domain
  )
  span
}

end_ospan <- function(span) {
  if (!is_otel_tracing()) {
    return(invisible())
  }

  otel::end_span(span)
}


# Start a span who will be stored on the session obect
create_domain_ospan <- function(
  name,
  ...,
  attributes = NULL,
  domain = getDefaultReactiveDomain()
) {
  if (!is_otel_tracing()) {
    return(NULL)
  }

  span <- otel_start_span(
    name,
    ...,
    attributes = attributes,
    domain = domain
  )

  # Make sure there is no existing span
  cur_span <- get_ospan_from_domain(name, domain = domain)
  stopifnot(is.null(cur_span))

  # Store span in session
  set_ospan_in_domain(name, span, domain = domain)

  # Add a failsafe to end the span
  end_forgotten_domain_ospans_on_session_end(domain)

  return(invisible())

  # return(function() {
  #   cli::cli_abort(
  #     "This span {.var name} was stored in the session. Please call {.fun end_session_ospan} to end it."
  #   )
  # })
}


# End a span who has been stored on the session obect
end_domain_ospan <- function(name, ..., domain = getDefaultReactiveDomain()) {
  stopifnot(length(list(...)) == 0)
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

  invisible()
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
#' @param ... Additional arguments passed to `create_ospan()`.
#' @param domain The reactive domain to associate with the span. Defaults to
#'   the current reactive domain from `getDefaultReactiveDomain()`.
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
#' promise <- with_ospan_async("async_operation", domain = domain, {
#'   # ... return a promise ...
#'   some_async_function()
#' })
#' }
#'
#' @seealso [`create_ospan`] for manual span control,
#'   [`with_existing_ospan_async`] for using existing spans
#' @keywords internal
with_ospan_async <- function(
  name,
  expr,
  ...,
  attributes = NULL,
  domain
) {
  if (!is_otel_tracing()) {
    return(force(expr))
  }

  # message("\n\nStarting ospan: ", name)
  span <- create_ospan(name, ..., attributes = attributes, domain = domain)

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


#' Execute code within an existing OpenTelemetry span for asynchronous operations
#'
#' Executes the given expression within the context of an existing OpenTelemetry
#' span that was previously stored in the reactive domain. This function retrieves
#' the span by name and sets up the appropriate promise domain for handling
#' asynchronous operations. Unlike `with_ospan_async`, this function does not
#' create a new span but uses an existing one.
#'
#' @param name Character string. The name of the existing span to retrieve from
#'   the reactive domain.
#' @param expr An expression to evaluate within the span context.
#' @param ... Currently unused.
#' @param domain The reactive domain to retrieve the span from. Defaults to
#'   the current reactive domain from `getDefaultReactiveDomain()`.
#'
#' @return The result of evaluating `expr` within the active span context.
#'
#' @details
#' This function is particularly useful for operations that need to run within
#' the context of a span that was created elsewhere and stored in the reactive
#' domain, such as reactive lock operations or observer runs. It properly sets
#' up the promise domain to ensure that asynchronous operations are correctly
#' traced.
#'
#' @examples
#' \dontrun{
#' # First, create and store a span in the domain
#' span <- create_ospan("parent_operation", domain = domain)
#'
#' # Later, use the existing span by name
#' result <- with_existing_ospan_async("parent_operation", {
#'   # ... do work within the existing span ...
#'   some_function()
#' }, domain = domain)
#'
#' # End the span when done
#' end_ospan(span)
#' }
#'
#' @seealso [`with_ospan_async`] for creating and using new spans,
#'   [`create_ospan`] for manual span creation
#' @keywords internal
with_existing_ospan_async <- function(
  name,
  expr,
  ...,
  domain
) {
  if (!is_otel_tracing()) {
    return(force(expr))
  }

  # Retrieve the existing span from the domain
  span <- get_ospan_from_domain(name, domain = domain)

  if (is.null(span)) {
    # If no span exists, just execute the expression
    return(force(expr))
  }

  with_ospan_promise_domain(span, expr)
}

# # TODO: Set attributes on the current active span
# # 5. Set attributes on the current active span
# set_ospan_attrs(status = 200L)


# -- Helpers --------------------------------------------------------------

has_existing_ospan <- function(name, domain = getDefaultReactiveDomain()) {
  !is.null(get_ospan_from_domain(name, domain = domain))
}


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
  # We need the current active span to be activated last, not first
  # Compose the new promise domain at the end, not beginning.
  new_domain <- promises:::compose_domains(
    act_span_pd,
    promises:::current_promise_domain()
  )

  # Don't overwrite the wrapSync method. Instead, call `otel::with_active_span()` directly
  # new_domain$wrapSync <- act_span_pd$wrapSync

  otel::with_active_span(span, {
    promises::with_promise_domain(new_domain, expr, replace = TRUE)
  })
}

otel_start_span <- function(
  name,
  ...,
  attributes = NULL,
  domain = getDefaultReactiveDomain()
) {
  otel::start_span(
    name,
    ...,
    attributes = otel::as_attributes(c(
      attributes,
      list(session = domain$token)
    ))
  )
}

otel_log_safe <- function(
  msg,
  ...,
  severity = "info",
  logger = NULL
) {
  # Use `"{msg}"` instead of `msg` to prevent otel from doing glue processing on the message
  otel::log("{msg}", ..., severity = severity, logger = logger)
}


get_ospan_names_from_domain <- function(domain = getDefaultReactiveDomain()) {
  names(domain$userData[["_otel"]])
}
get_ospan_from_domain <- function(
  span_name,
  domain = getDefaultReactiveDomain()
) {
  env = domain$userData
  if (is.null(env)) {
    return(NULL)
  }

  get0("_otel", envir = env)[[span_name]]
}
set_ospan_in_domain <- function(
  span_name,
  span,
  domain = getDefaultReactiveDomain()
) {
  if (is.null(get0("_otel", envir = domain$userData))) {
    domain$userData[["_otel"]] <- list()
  }
  otel_info <- domain$userData[["_otel"]]
  otel_info[[span_name]] <- span
  domain$userData[["_otel"]] <- otel_info
}
