otel_collect_choices <- c(
  "none",
  "session",
  "reactive_update",
  "reactivity",
  "all"
)

# Check if the collect level is sufficient
otel_collect_is_enabled <- function(
  impl_level,
  # Listen to option and fall back to the env var
  opt_collect_level = getOption("shiny.otel.collect", Sys.getenv("SHINY_OTEL_COLLECT", "all"))
) {
  opt_collect_level <- as_otel_collect(opt_collect_level)

  which(opt_collect_level == otel_collect_choices) >=
    which(impl_level == otel_collect_choices)
}

# Check if tracing is enabled and if the collect level is sufficient
has_otel_collect <- function(collect) {
  # Only check pkg author input iff loaded with pkgload
  if (IS_SHINY_LOCAL_PKG) {
    stopifnot(length(collect) == 1, any(collect == otel_collect_choices))
  }

  otel_is_tracing_enabled() && otel_collect_is_enabled(collect)
}

# Run expr with otel collection disabled
with_no_otel_collect <- function(expr) {
  withr::with_options(
    list(
      shiny.otel.collect = "none"
    ),
    expr
  )
}


## -- Helpers -----------------------------------------------------

# shiny.otel.collect can be:
# "none"; To do nothing / fully opt-out
# "session" for session/start events
# "reactive_update" (includes "session" features) and reactive_update spans
# "reactivity" (includes "reactive_update" features) and spans for all reactive things
# "all" - Anything that Shiny can do. (Currently equivalent to the "reactivity" level)

as_otel_collect <- function(collect = "all") {
  if (!is.character(collect)) {
    stop("`collect` must be a character vector.")
  }

  # Match to collect enum
  collect <- match.arg(collect, otel_collect_choices, several.ok = FALSE)

  return(collect)
}


# ------------------------------------------

# # Approach
# Use flags on the reactive object to indicate whether to record OpenTelemetry spans.
#
# Cadence:
# * `$.isRecordingOtel` - Whether to record OpenTelemetry spans for this reactive object
# * `$.otelLabel` - The label to use for the OpenTelemetry span
# * `$.otelAttrs` - Additional attributes to add to the OpenTelemetry span


#' Add OpenTelemetry for reactivity to an object
#'
#' @description
#'
#' `enable_otel_*()` methods add OpenTelemetry flags for [reactive()] expressions
#' and `render*` functions (like [renderText()], [renderTable()], ...).
#'
#' Wrapper to creating an active reactive OpenTelemetry span that closes when
#' the reactive expression is done computing. Typically this is when the
#' reactive expression finishes (synchronous) or when the returned promise is
#' done computing (asynchronous).

#' @section Async with OpenTelemetry:
#'
#'   With a reactive expression, the key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises --- not regular R
#'   promises, but rather objects provided by the
#'   \href{https://rstudio.github.io/promises/}{\pkg{promises}}  package, which
#'   are similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.) You can also use [mirai::mirai()] or [future::future()]
#'   objects to run code in a separate process or even on a remote machine.
#'
#'   When reactive expressions are being calculated in parallel (by having
#'   another reactive promise compute in the main process), the currently active
#'   OpenTelemetry span will be dynamically swapped out according to the
#'   currently active reactive expression. This means that as long as a promise
#'   was `then()`ed or `catch()`ed with an active OpenTelemetry span, the span
#'   will be correctly propagated to the next step (and subsequently other
#'   steps) in the promise chain.
#'
#'   While the common case is for a reactive expression to be created
#'   synchronously, troubles arise when the reactive expression is created
#'   asynchronously. The span **must** be created before the reactive expression
#'   is executed, it **must** be active for the duration of the expression, and
#'   it **must** not be closed until the reactive expression is done executing.
#'   This is not easily achieved with a single function call, so we provide a
#'   way to create a reactive expression that is bound to an OpenTelemetry
#'   span.
#'
#' @section Span management and performance:
#'
#' Dev note - Barret 2025-10:
#' Typically, an OpenTelemetry span (`otel_span`) will inherit from the parent
#' span. This works well and we can think of the hierarchy as a tree. With
#' `options("shiny.otel.collect" = <value>)`, we are able to control with a sliding
#' dial how much of the tree we are interested in: "none", "session",
#' "reactive_update", "reactivity", and finally "all".
#'
#' Leveraging this hierarchy, we can avoid creating spans that are not needed.
#' The act of making a noop span takes on the order of 10microsec. Handling of
#' the opspan is also in the 10s of microsecond range. We should avoid this when
#' we **know** that we're not interested in the span.  Therefore, manually
#' handling spans should be considered for Shiny.
#'
#' * Q:
#'   * But what about app author who want the current span? Is there any
#'     guarantee that the current span is expected `reactive()` span?
#' * A:
#'   * No. The current span is whatever the current span is. If the app author
#'     wants a specific span, they should create it themselves.
#' * Proof:
#'   ```r
#'   noop <- otel::get_active_span()
#'   noop$get_context()$get_span_id()
#'   #> [1] "0000000000000000"
#'   ignore <- otelsdk::with_otel_record({
#'     a <- otel::start_local_active_span("a")
#'     a$get_context()$get_span_id() |> str()
#'     otel::with_active_span(noop, {
#'       otel::get_active_span()$get_context()$get_span_id() |> str()
#'     })
#'   })
#'   #> chr "2645e95715841e75"
#'   #> chr "2645e95715841e75"
#'   # ## It is reasonable to expect the second id to be `0000000000000000`, but it's not.
#'   ```
#'   Therefore, the app author has no guarantee that the current span is the
#'   span they're expecting. If the app author wants a specific span, they should
#'   create it themselves and let natural inheritance take over.
#'
#' Given this, I will imagine that app authors will set
#' `options("shiny.otel.collect" = "reactive_update")` as their default behavior.
#' Enough to know things are happening, but not overwhelming from **everything**
#' that is reactive.
#'
#' To _light up_ a specific area, users can call `withr::with_options(list("shiny.otel.collect" = "all"), { ... })`.
#'
#' @param x The object to add caching to.
#' @param ... Future parameter expansion.
#' @noRd
NULL


enable_otel_reactive_val <- function(x) {

  impl <- attr(x, ".impl", exact = TRUE)
  # Set flag for otel logging when setting the value
  impl$.isRecordingOtel <- TRUE

  class(x) <- c("reactiveVal.otel", class(x))

  x
}

enable_otel_reactive_values <- function(x) {

  impl <- .subset2(x, "impl")
  # Set flag for otel logging when setting values
  impl$.isRecordingOtel <- TRUE

  class(x) <- c("reactivevalues.otel", class(x))

  x
}

enable_otel_reactive_expr <- function(x) {

  domain <- reactive_get_domain(x)

  impl <- attr(x, "observable", exact = TRUE)
  impl$.isRecordingOtel <- TRUE
  # Covers both reactive and reactive.event
  impl$.otelLabel <- otel_span_label_reactive(x, domain = impl$.domain)

  class(x) <- c("reactiveExpr.otel", class(x))

  x
}

enable_otel_observe <- function(x) {
  x$.isRecordingOtel <- TRUE
  x$.otelLabel <- otel_span_label_observer(x, domain = x$.domain)

  class(x) <- c("Observer.otel", class(x))
  invisible(x)
}



enable_otel_shiny_render_function <- function(x) {

  valueFunc <- force(x)
  otel_span_label <- NULL
  otel_span_attrs <- NULL

  renderFunc <- function(...) {
    # Dynamically determine the span label given the current reactive domain
    if (is.null(otel_span_label)) {
      domain <- getDefaultReactiveDomain()
      otel_span_label <<-
        otel_span_label_render_function(x, domain = domain)
      otel_span_attrs <<- c(
        attr(x, "otelAttrs"),
        otel_session_id_attrs(domain)
      )
    }

    with_otel_span(
      otel_span_label,
      {
        hybrid_then(
          valueFunc(...),
          on_failure = set_otel_exception_status_and_throw,
          # Must save the error object
          tee = FALSE
        )
      },
      attributes = otel_span_attrs
    )
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- c("shiny.render.function.otel", class(valueFunc))
  renderFunc
}


# ## If we ever expose a S3 function, I'd like to add this method.
# bindOtel.function <- function(x, ...) {
#   cli::cli_abort(paste0(
#     "Don't know how to add OpenTelemetry recording to a plain function. ",
#     "If this is a {.code render*()} function for Shiny, it may need to be updated. ",
#     "Please see {.help shiny::bindOtel} for more information."
#   ))
# }
