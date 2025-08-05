# - Goals -----------------------------------
# * Integration locations:
#   * Server:
#     * Start when reactive busy count > 0; Close when reactive busy count == 0
#   * Reactives:
#     * Reactive value - `reactiveVal`
#     * Reactive values - `reactiveValues`
#     * Reactive expression - `reactive`
#     * Render function - `shiny.render.function`
# * Global options:
#   * shiny.otel.graphlocked: TRUE/FALSE - whether to lock the graph for
#     OpenTelemetry spans, so that they are not modified while being traced.
#   * shiny.otel.bindall: TRUE/FALSE - whether to bind all reactive objects
#     to OpenTelemetry spans. If TRUE, every reactive objects
#     are bound to OpenTelemetry spans
# * Methods:
#   * bindOtel() - S3 method that binds the reactive object to OpenTelemetry spans
#     * Note: When adding otel to an object, prepend a class of `FOO.otel`. Then add a dispatch method for `bindOtel.FOO.otel()` that declares the object already has been bound.
#   * withOtelShiny() - runs the expression with OpenTelemetry spans enabled


# - Questions -----------------------------------
# TODO: freeze / thaw? - log restart event?
# TODO: reactiveTimer / invalidateLater / reactivePoll / reactiveFileReader
# TODO: observeEvent / eventReactive - I don't believe these are possible except when binding all otel
# TODO: debounce / throttle - I don't believe these are possible except when binding all otel


# Personal debugging function -------------------------------
# system("air format ./R/bind-otel.R"); devtools::load_all(); barret() |> runApp(port = 8080)

# TODO: Remove this function when done debugging
barret <- function() {
  options(
    shiny.otel.graphlocked = TRUE,
    shiny.otel.bindall = TRUE
  )

  # Inspiration from
  # * https://github.com/r-lib/otel/commit/a2ef493ae4b97701e4e178ac527f313580539080
  # * https://github.com/r-lib/otel/commit/09c0eb6c80d5b907976de8fbaf89798cb11f8e6e#diff-169b8f234d0b208affb106fce375f86fefe2f16dba4ad66495a1dc06c8a4cd7b

  otel_tracer <- otel::get_tracer("kmeans-shiny-app")
  otel_start_shiny_session <- function(session) {
    message("Starting OpenTelemetry session for Shiny app")
    otel_tracer$start_span("app")
    session$userData[["otel_tracer"]] <- otel_tracer
    session$userData[["otel_span"]] <-
      otel::start_local_active_span(
        "session",
        tracer = otel_tracer,
        # scope = NULL,
        attributes = otel_session_attr(session),
        end_on_exit = FALSE
      )
  }
  otel_session_attr <- function(session) {
    attr <- list(
      PATH_INFO = session[["request"]][["PATH_INFO"]] %||% "",
      HTTP_HOST = session[["request"]][["HTTP_HOST"]] %||% "",
      HTTP_ORIGIN = session[["request"]][["HTTP_ORIGIN"]] %||% "",
      QUERY_STRING = session[["request"]][["QUERY_STRING"]] %||% "",
      SERVER_PORT = session[["request"]][["SERVER_PORT"]] %||% ""
    )
    try(attr[["SERVER_PORT"]] <- as.integer(attr[["SERVER_PORT"]]))

    session$onSessionEnded(function(...) {
      session$userData$otel_span$end()
    })

    attr
  }

  shinyApp(
    ui = fluidPage(
      sliderInput("x", "x", 1, 10, 5),
      sliderInput("y", "y", 1, 10, 5),
      div("x * y: "),
      verbatimTextOutput("txt")
    ),
    server = function(input, output, session) {
      # shiny::bindOtel(TRUE)

      otel_start_shiny_session(session)
      otel::log_info("Start new Shiny session")
      # otel_cnt_session$add()

      r <- reactive({
        message("Doing expensive computation...")
        Sys.sleep(1)
        input$x * input$y
      })
      #  |>
      #   bindOtel()
      # Q: Automatically sets x = isolate(input$x), y = isolate(input$y) as otel attributes?
      # * Ans: No. Values could be HUGE, so we don't want to do that.
      # Q: Manually accept reactive expressions to isolate on during otel span creation?
      # * Ans: Maybe? Let's leave a door open for that via `bindOtel(...)`.

      output$txt <-
        renderText(r())
      # |>
      # bindOtel()
    }
  )
}


# ------------------------------------------

utils::globalVariables(".GenericCallEnv", add = TRUE)

# TODO: Maybe a top level option to set the defaults? `shiny.otel = TRUE`?
is_binding_all_otel <- function() {
  getOption("shiny.otel.bindall", FALSE)
}
is_recording_otel_reactive_graph_lock <- function() {
  getOption("shiny.otel.graphlocked", FALSE)
}

#' Set OpenTelemetry options for Shiny reactives
#'
#' @param expr The expression to run with OpenTelemetry spans enabled.
#' @param ... Future parameter expansion.
#' @param bindAll If `TRUE`, then all reactive objects will be bound to Open Telemetry spans.
#'   If `FALSE`, then only the reactive objects created within the expression
#'   will be bound to Open Telemetry spans.
#'   Defaults to `TRUE`.
#' @export
#' @examples
#' # TODO: Update examples!!
withOtelShiny <- function(expr, ..., bindAll = TRUE) {
  rlang::check_dots_empty()

  withr::with_options(
    list(
      shiny.otel.bindall = isTRUE(bindAll)
    ),
    expr
  )
}

#' Add Open Telemetry for reactivity to an object
#'
#' @description
#'
#' `bindOtel()` adds Open Telemetry for [reactive()] expressions and `render*`
#' functions (like [renderText()], [renderTable()], ...).
#'
#' Wrapper to creating an active reactive Open Telemetry span that closes when
#' the reactive expression is done computing. Typically this is when the
#' reactive expression finishes (synchronous) or when the returned promise is
#' done computing (asynchronous).

#' @section Async with Open Telemetry:
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
#'   Open Telemetry span will be dynamically swapped out according to the
#'   currently active reactive expression. This means that as long as a promise
#'   was `then()`ed or `catch()`ed with an active Open Telemetry span, the span
#'   will be correctly propagated to the next step (and subsequently other
#'   steps) in the promise chain.
#'
#'   While the common case is for a reactive expression to be created
#'   synchronously, troubles arise when the reactive expression is created
#'   asynchronously. The span **must** be created before the reactive expression
#'   is executed, it **must** be active for the duration of the expression, and
#'   it **must** not be closed until the reactive expression is done executing.
#'   This is not easily achieved with a single function call, so we provide a
#'   way to create a reactive expression that is bound to an Open Telemetry
#'   span.
#'
#' @param x The object to add caching to.
#' @param ... Future parameter expansion.
#' @seealso [bindCache()] and [bindEvent()] for other ways to bind to your reactives.
#'
#' @examples
#' # TODO: Update examples!!
#' \dontrun{
#' rc <- bindCache(
#'   x = reactive({
#'     Sys.sleep(2)   # Pretend this is expensive
#'     input$x * 100
#'   }),
#'   input$x
#' )
#'
#' # Can make it prettier with the %>% operator
#' library(magrittr)
#'
#' rc <- reactive({
#'   Sys.sleep(2)
#'   input$x * 100
#' }) %>%
#'   bindCache(input$x)
#'
#' }
#'
#' ## Only run app examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- reactive({
#'       # The value expression is an _expensive_ computation
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y)
#'
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#'
#' # Caching renderText
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     output$txt <- renderText({
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y)
#'   }
#' )
#'
#'
#' # Demo of using events and caching with an actionButton
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     actionButton("go", "Go"),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- reactive({
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y) %>%
#'       bindEvent(input$go)
#'       # The cached, eventified reactive takes a reactive dependency on
#'       # input$go, but doesn't use it for the cache key. It uses input$x and
#'       # input$y for the cache key, but doesn't take a reactive dependency on
#'       # them, because the reactive dependency is superseded by addEvent().
#'
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#' }
#'
#' @export
bindOtel <- function(x, ...) {
  UseMethod("bindOtel")
}

#' @export
bindOtel.default <- function(x, ...) {
  stop(
    "Don't know how to handle object with class ",
    paste(class(x), collapse = ", ")
  )
}

#' @export
bindOtel.reactiveVal <- function(x, ...) {
  check_dots_unnamed()

  impl <- attr(x, ".impl", exact = TRUE)
  impl$.is_logging_otel <- TRUE

  class(x) <- c("reactiveVal.otel", class(x))

  x
}

#' @export
bindOtel.reactiveValues <- function(x, ...) {
  check_dots_unnamed()

  impl <- attr(x, ".impl", exact = TRUE)
  impl$.is_logging_otel <- TRUE

  class(x) <- c("reactiveValues.otel", class(x))

  x
}

#' @export
bindOtel.reactiveExpr <- function(x, ...) {
  check_dots_unnamed()

  label <- exprToLabel(substitute(key), "otel")
  domain <- reactive_get_domain(x)

  valueFunc <- reactive_get_value_func(x)
  valueFunc <- wrapFunctionLabel(
    valueFunc,
    "otelReactiveValueFunc",
    ..stacktraceon = TRUE
  )

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)
  # Hacky workaround for issue with `%>%` preventing GC:
  # https://github.com/tidyverse/magrittr/issues/229
  if (exists(".GenericCallEnv") && exists(".", envir = .GenericCallEnv)) {
    rm(list = ".", envir = .GenericCallEnv)
  }

  # Turn off binding all otel, so that we don't recursively bind forever
  # `withOtelShiny()` does not virally enable/disable binding all otel,
  # only in "this" tick
  withOtelShiny(bindAll = FALSE, {
    res <- reactive(label = label, domain = domain, {
      generateOtelFun(
        {
          valueFunc()
        },
        "reactiveExpr",
        attributes = list(
          label = label
        ),
        domain = domain
      )()
    })
  })

  class(res) <- c("reactive.otel", class(res))
  res
}

#' @export
bindOtel.shiny.render.function <- function(x, ...) {
  check_dots_unnamed()

  valueFunc <- x

  otel_dots <- list(...)

  renderFunc <- function(...) {
    domain <- getDefaultReactiveDomain()

    generateOtelFun(
      {
        valueFunc(...)
      },
      "shiny.render.function",
      # !!!otel_dots,
      # attributes = NULL,
      # links = NULL,
      # option = NULL,
      domain = domain
    )()
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- c("shiny.render.function.otel", class(valueFunc))
  renderFunc
}


#' @export
bindOtel.reactive.event <- function(x, ...) {
  stop(
    "Can't call bindOtel() after calling bindEvent() on an object. Maybe you wanted to call bindEvent() after bindOtel()?"
  )
}


#' @export
bindOtel.Observer <- function(x, ..., label = NULL) {
  if (x$.execCount > 0) {
    stop(
      "Cannot call bindOtel() on an Observer that has already been executed."
    )
  }

  qs <- enquos0(...)
  # eventFunc <- quos_to_func(qs)
  obsFunc <- x$.func

  # Note that because the observer will already have been logged by this point,
  # this updated label won't show up in the reactlog.
  x$.label <- label %||%
    sprintf('bindOtel(%s, %s)', x$.label, quos_to_label(qs))

  x$.func <- wrapFunctionLabel(
    name = x$.label,
    ..stacktraceon = FALSE,
    func = function() {
      generateOtelFun(
        {
          obsFunc()
        },
        "bindOtel.Observer",
        attributes = list(label = x$.label),
        links = NULL,
        option = NULL,
        domain = x$.domain
      )()
    }
  )

  class(x) <- c("Observer.otel", class(x))
  invisible(x)
}


#' @export
bindEvent.reactive.otel <- function(x, ...) {
  # TODO: What is the proper support for bindEvent() and bindOtel()?
  stop(
    "bindEvent() has already been called on the object attempting to add OpenTelemetry."
  )
}


#' @export
bindOtel.function <- function(x, ...) {
  cli::cli_abort(paste0(
    "Don't know how to add Open Telemetry recording to a plain function. ",
    "If this is a {.code render*()} function for Shiny, it may need to be updated. ",
    "Please see {.help shiny::bindOtel} for more information."
  ))
}

# Returns a function which should be passed as a step in to hybrid_chain()
generateOtelFun <- function(
  expr,
  name,
  ...,
  attributes = NULL,
  links = NULL,
  option = NULL,
  domain = getDefaultReactiveDomain()
) {
  function() {
    is_tracing <- otel::is_tracing_enabled()
    with_otel_active_span_promise_domain <- rlang::ns_env("promises")[["with_otel_active_span_promise_domain"]]
    hybrid_chain(
      {
        # Use a placeholder to indicate that otel is tracing.
        # This way we shut down only the spans that we create
        if (is_tracing) {
          # Create an inactive span
          # inactive_span <- promises:::otel_create_inactive_span(
          active_span <-
            otel_start_active_shiny_span(
              domain,
              name,
              ...,
              attributes = attributes,
              links = links,
              option = option
            )

          # Use the local promise domain
          with_otel_active_span_promise_domain(
            active_span,
            force(expr)
          )
        } else {
          force(expr)
        }
      },
      finally = function() {
        if (is_tracing) {
          # This will run after the valueFunc() final promise is resolved
          otel::end_span(active_span)
        }
      }
    )
  }
}

# - Double binding checks -----------------------------

#' @export
bindOtel.reactive.otel <- function(x, ...) {
  stop("bindOtel() has already been called on the object.")
}
#' @export
bindOtel.reactiveVal.otel <- bindOtel.reactive.otel
#' @export
bindOtel.reactiveValues.otel <- bindOtel.reactive.otel
#' @export
bindOtel.shiny.render.function.otel <- bindOtel.reactive.otel
#' @export
bindOtel.shiny.render.function.event <- bindOtel.reactive.event
#' @export
bindOtel.Observer.otel <- bindOtel.reactive.otel


# - Create Shiny Domain otel span ---------------------------

otel_start_active_shiny_span <- function(domain, name, ..., attributes = NULL) {
  otel::start_local_active_span(
    name,
    tracer = domain$userData[["otel_tracer"]],
    ...,
    attributes = otel::as_attributes(c(
      attributes,
      list(session = domain$token)
    )),
    end_on_exit = FALSE
  )
}
