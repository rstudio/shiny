# - Goals -----------------------------------
# * Integration locations:
#   * Server:
#     * Start when reactive busy count > 0; Close when reactive busy count == 0
#   * Reactives:
#     * Reactive value - `reactiveVal`
#     * Reactive values - `reactiveValues`
#     * Reactive expression - `reactive`
#     * Render function - `shiny.render.function`
#     * Observer - `observe`
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

# - TODO -----------------------------------
# * Tests with otel recording
`_ignore` <- function() {
  otelsdk::with_otel_record
}


# - Questions -----------------------------------
# TODO: freeze / thaw? - log restart event?
# TODO: reactiveTimer / invalidateLater / reactivePoll / reactiveFileReader
# TODO: observeEvent / eventReactive - I don't believe these are possible except when binding all otel
# TODO: debounce / throttle - I don't believe these are possible except when binding all otel
# TODO: Extended Tasks are linked from parent span. Maybe use an envvar for span context? It is allowed for child process can end much later than the parent process. Take inspiration from callr PR (copying of the span context to the child process).

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

  otel_tracer <- otel::get_tracer("kmeans-shiny-app-v2")

  otel_start_shiny_session <- function(
    session,
    activation_scope = parent.frame()
  ) {
    session$userData[["otel_tracer"]] <- otel_tracer
    start_session_ospan(
      OSPAN_SESSION_NAME,
      tracer = otel_tracer,
      attributes = otel_session_attr(session),
      domain = session
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
      with_existing_ospan_async(
        OSPAN_SESSION_NAME,
        {
          otel::log_info("Start new Shiny session")
        },
        domain = session
      )

      x <- reactive({
        x_val <- input$x
        otel_log_safe("X Val: {x_val}")
        Sys.sleep(0.5)
        x_val
      })
      y <- reactive({
        y_val <- input$y
        otel_log_safe("Y Val: {y_val}")
        Sys.sleep(0.5)
        y_val
      })

      calc <- reactive({
        message("Doing expensive computation...")
        x() * y()
      })
      #  |>
      #   bindOtel(
      #     label = "Expensive calc"
      #     # # name = "Expensive calc!!",
      #     # attributes = \() {
      #     #   list(x = input$x, y = input$y)
      #     # }
      #     # # ,
      #     # name_x = {input$x},
      #     # name_y = {input$y}
      #   )
      #  |>
      #   bindOtel()
      # Q: Automatically sets x = isolate(input$x), y = isolate(input$y) as otel attributes?
      # * Ans: No. Values could be HUGE, so we don't want to do that.
      # Q: Manually accept reactive expressions to isolate on during otel span creation?
      # * Ans: Maybe? Let's leave a door open for that via `bindOtel(...)`.

      # observe({
      #   message("x: ", x())
      # })

      # output$txt <- renderText({
      #   calc()
      # })

      log_and_msg <- function(...) {
        msg <- paste(...)
        message(msg)
        otel::log_info(msg)
      }

      x_prom <- reactive({
        x_span_id <- force(otel::get_active_span_context()$get_span_id())
        # message("x_prom span id: ", x_span_id)
        x_val <- force(input$x)
        log_and_msg("x_prom init")
        promises::promise(\(resolve, reject) {
          log_and_msg("x_prom 0")
          resolve(x_val)
        }) |>
          promises::then(function(x_val) {
            log_and_msg("x_prom 1")
            x_val
          }) |>
          promises::then(function(x_val) {
            log_and_msg("x_prom 2")
            x_val
          }) |>
          promises::then(function(x_val) {
            log_and_msg("x_prom 3")
            x_val
          })
      })

      y_prom <- reactive({
        y_span_id <- force(otel::get_active_span_context()$get_span_id())
        # message("y_prom span id: ", y_span_id)
        y_val <- force(input$y)
        log_and_msg("y_prom init")
        promises::promise(\(resolve, reject) {
          log_and_msg("y_prom 0")
          resolve(y_val)
        }) |>
          promises::then(function(y_val) {
            log_and_msg("y_prom 1")
            y_val
          }) |>
          promises::then(function(y_val) {
            log_and_msg("y_prom 2")
            y_val + calc()
          }) |>
          promises::then(function(y_val) {
            log_and_msg("y_prom 3")
            y_val
          })
      })

      observe(label = "proms_observer", {
        p <- promises::promise_all(
          x_prom(),
          y_prom()
        )

        promises::then(p, \(vals) {
          message("Vals[1]: ", vals[[1]])
          message("Vals[2]: ", vals[[2]])

          # Shut down the app so the telemetry can be seen easily
          later::later(
            \() {
              message("\n\nClosing session for tight graphs")
              session$close()
            },
            delay = 10 / 1000
          )
        })
      })

      # |>
      # bindOtel()
    }
  )
}


# ------------------------------------------

utils::globalVariables(".GenericCallEnv", add = TRUE)

# TODO: Maybe a top level option to set the defaults? `shiny.otel = TRUE`?
is_binding_all_otel <- function() {
  otel::is_tracing_enabled() && getOption("shiny.otel.bindall", FALSE)
}
is_recording_otel_reactive_graph_lock <- function() {
  otel::is_tracing_enabled() && getOption("shiny.otel.graphlocked", FALSE)
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
localOtelShiny <- function(..., bindAll = TRUE, .envir = parent.frame()) {
  rlang::check_dots_empty()

  withr::local_options(
    list(
      shiny.otel.bindall = isTRUE(bindAll)
    ),
    .envir = .envir
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
  rlang::check_dots_empty()

  impl <- attr(x, ".impl", exact = TRUE)
  impl$.is_logging_otel <- TRUE

  class(x) <- c("reactiveVal.otel", class(x))

  x
}

#' @export
bindOtel.reactiveValues <- function(x, ...) {
  rlang::check_dots_empty()

  impl <- attr(x, ".impl", exact = TRUE)
  impl$.is_logging_otel <- TRUE

  class(x) <- c("reactiveValues.otel", class(x))

  x
}

#' @export
bindOtel.reactiveExpr <- function(x, ...) {
  rlang::check_dots_empty()

  # TODO: This value feels wrong. `bindCache()` is also probably wrong.
  # label <- exprToLabel(substitute(key), "otel")
  label <- attr(x, "observable", exact = TRUE)[[".label"]]
  domain <- reactive_get_domain(x)

  valueFunc <- reactive_get_value_func(x)
  valueFunc <- wrapFunctionLabel(
    valueFunc,
    # TODO: Better name
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
      with_ospan_async(
        paste0("reactive(", label, ")"),
        {
          valueFunc()
        },
        domain = domain
      )
    })
  })

  class(res) <- c("reactive.otel", class(res))
  res
}

#' @export
bindOtel.shiny.render.function <- function(x, ...) {
  rlang::check_dots_empty()

  valueFunc <- x

  # str(x)

  # browser()

  renderFunc <- function(...) {
    with_ospan_async(
      # TODO: Better label
      paste0("shiny.render.function"),
      {
        valueFunc(...)
      }
    )
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
  rlang::check_dots_empty()

  if (x$.execCount > 0) {
    stop(
      "Cannot call bindOtel() on an Observer that has already been executed."
    )
  }

  # eventFunc <- quos_to_func(qs)
  obsFunc <- x$.func

  # Note that because the observer will already have been logged by this point,
  # this updated label won't show up in the reactlog.
  x$.label <- label %||%
    sprintf('observe(%s)', x$.label)

  x$.func <- wrapFunctionLabel(
    name = x$.label,
    ..stacktraceon = FALSE,
    func = function() {
      with_ospan_async(x$.label, {
        # browser()
        force(obsFunc())
      })
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
