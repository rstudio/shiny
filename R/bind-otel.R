# - Goals -----------------------------------
# * Integration locations:
#   * Server:
#     * √ Start when reactive busy count > 0
#     * √ Close when reactive busy count == 0
#   * Reactives:
#     * √ Reactive value - `reactiveVal`
#     * √ Reactive values - `reactiveValues`
#     * √ Reactive expression - `reactive`
#     * √ Render function - `shiny.render.function`
#     * √ Observer - `observe`
#   * Combinations:
#     * debounce() / throttle()
#     * bindCache()
#     * bindEvent()
#     * bindProgress()?
#   * Special functions:
#     * ExtendedTask()
#       * Extended task links to submission reactive
#       * Reactive update that gets result links to the extended task
#     * observeEvent()
#     * eventReactive()
#   * Maybe enhance all `withReactiveDomain()` calls?
# * Global options:
#   * √ shiny.otel.graphlocked: TRUE/FALSE - whether to lock the graph for
#     OpenTelemetry spans, so that they are not modified while being traced.
#   * √ shiny.otel.bind:
#     * "all", "none" - all or nothing
#     * Currently non-public options:
#       * "reactiveVal", "reactiveValues", "reactiveExpr", "observe", "output" - corresponding reactive objects
#       * "reactive_update" - Surrounds a reactive update. Shiny is "busy"
#       * "session" - Surrounds the app `server()` function
# * Methods:
#   * bindOtel() - S3 method that binds the reactive object to OpenTelemetry spans
#     * Note: When adding otel to an object, prepend a class of `FOO.otel`. Then add a dispatch method for `bindOtel.FOO.otel()` that declares the object already has been bound.
#   * withOtel(expr, ..., bind) - runs the expression with OpenTelemetry spans enabled

# - TODO -----------------------------------
# * √ Nerf bind options to just `"all"` and `"none"`
# * √ Labels `reactive_update`
# * √ Value `reactive-update` -> `reactive_update`
# * √ Labels: (apply to others accordingly)
#   * observe mymod:<anonymous>
#   * observe <anonymous>
#   * observe mylabel (edited)
# * Add code file/line markers when we are able discover the name from the srcinfo
# * Session
#   * Move Session span to a log start and log end: https://opentelemetry.io/docs/specs/semconv/registry/attributes/session/
#   * Add `session.id` with the token to attrs
# * Connect `user.id` to be their user name: https://opentelemetry.io/docs/specs/semconv/registry/attributes/user/
# * bindOtel() should no-op when binding an already bound object (where as currently it throws)
# * Tests with otel recording
# * √ Errors should happen in the span only

`_ignore` <- function() {
  otelsdk::with_otel_record
}


# - Questions -----------------------------------
# Add error handling for every otel. Use withCallingHandlers similar to https://github.com/r-lib/mirai/pull/395/files#diff-9e809582679952a93b9f34755bb38207471945eb36cedb9e2aa755125449f531R214-R215
# TODO: log events for bookmark?
  # Ans: Seems possibly excessive in amount
# TODO: log events like fatal errors? / onUnhandledError
  # Good idea!

# TODO: Add spans for session callbacks? onRestore/onRestored, onSessionEnded, onUnhandledError, on any callback for the session
  # Ans: It is the user's responsiblity to add spans for these methods
# TODO: freeze / thaw? - log restart event?
# TODO: reactiveTimer / invalidateLater / reactivePoll / reactiveFileReader
# TODO: Extended Tasks are linked from parent span. Maybe use an envvar for span context? It is allowed for child process can end much later than the parent process. Take inspiration from callr PR (copying of the span context to the child process).

# Personal debugging function -------------------------------
# system("air format ./R/bind-otel.R"); devtools::load_all(); barret() |> runApp(port = 8080)

# TODO: Remove this function when done debugging
barret <- function() {
  # library(mirai)
  # daemons(2)

  # Inspiration from
  # * https://github.com/r-lib/otel/commit/a2ef493ae4b97701e4e178ac527f313580539080
  # * https://github.com/r-lib/otel/commit/09c0eb6c80d5b907976de8fbaf89798cb11f8e6e#diff-169b8f234d0b208affb106fce375f86fefe2f16dba4ad66495a1dc06c8a4cd7b

  # TODO: Maybe the name is the folder name, similar to shinyapps.io naming
  # Maybe set from a function call somewhere?
  # otel_tracer <- otel::get_tracer("my-app")
  otel_logger <- otel::get_logger("my-app-logger")
  # options("shiny.otel.tracer" = otel_tracer)

  withr::with_environment(globalenv(), {
    otel_tracer_name <- "my-app"
  })

  log_and_msg <- function(..., .envir = parent.frame()) {
    msg <- paste(...)
    message(msg)
    # otel::log_info(msg, tracer = session$userData[["_otel_tracer"]])
    # TODO try to remove the logger param once function is removed from Shiny package
    otel_log_safe(msg, logger = otel_logger)
    # otel_log_safe(msg)
  }

  app <- shinyApp(
    ui = fluidPage(
      sliderInput("mymod-x", "x", 1, 10, 5),
      sliderInput("mymod-y", "y", 1, 10, 5),
      div("x * y: "),
      verbatimTextOutput("mymod-txt")
    ),
    server = function(input, output, session) {

      log_and_msg("Start new Shiny session")

      b <- reactiveVal(1)
      observe(b(42))

      # shiny::bindOtel(TRUE)


      shutdown <- function() {
        later::later(
          function() {
            message("\n\nClosing session for minimal logfire graphs")
            session$close()
            # mirai::daemons(0)
          },
          delay = 10 / 1000
        )
      }

      xMod <- function(id) {
        moduleServer(id, function(input, output, session) {
          xVal <- reactiveVal(NULL)

          # with_existing_ospan_async(
          #   OSPAN_SESSION_NAME,
          #   {
          log_and_msg("Shiny module")
          #   },
          #   domain = session
          # )

          # x_raw <- reactive({
          bar <<- x <- reactive({
            x_val <- xVal()
            req(x_val)
            log_and_msg(sprintf("X Val: %s", x_val))
            # Sys.sleep(0.5)
            x_val
          })
          # x <- debounce(x_raw, 100)
          y <- reactive({
            y_val <- input$y
            log_and_msg(sprintf("Y Val: %s", y_val))
            # Sys.sleep(0.5)
            y_val
          })

          calc <- reactive(label = "barret_calc", {
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

          # observe(label = "barret_observe", {
          observe({
            message("x: ", x())
          })

          observe(message("longer message. y: ", y()))

          output$txt <- renderText({

            # lapply(1:10, function(i) {
            #   otel::start_local_active_span("my_span")
            #   Sys.sleep(0.1)
            #   otel::log_info("Extra info: {q()}")
            #   my_val <- "q()"
            #   otel::log_info("Extra info: {my_val}")
            # })

            # my_val <- 42
            # otel::log_info("Extra info: {my_val}")

            # this_span <- otel::get_active_span_context()$current_span()
            # this_span$set_attribute("key", val)

            # otel::start_local_active_span("output(mymod-txt)", attributes = otel::as_attributes(list(session = getDefaultReactiveDomain(()), file = "bind-otel.R", file_line = 164)))
            calc()
          })

          x_prom <- reactive({
            x_span_id <- force(otel::get_active_span_context()$get_span_id())
            # message("x_prom span id: ", x_span_id)
            x_val <- x()
            log_and_msg("x_prom init")
            promises::promise(\(resolve, reject) {
              log_and_msg("x_prom 0")
              resolve(x_val)
            }) |>
              promises::then(function(x_val) {
                log_and_msg("x_prom 1")
                log_and_msg("Launching mirai")
                x_val
                # mirai_map(seq_len(x_val), \(i) {
                #   otel::start_local_active_span("slow compute")
                #   Sys.sleep(i / 10 / 100)
                #   i
                # }) |>
                #   promises::then(\(vals) {
                #     max(unlist(vals))
                #   })

                # # mirai(
                # #   {
                # #     otel::start_local_active_span("slow compute")
                # #     # val
                # #     # Sys.sleep(0.2)
                # #     val
                # #   },
                # #   val = x_val
                # # )
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
            y_val <- y()
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
            promises::then(p, function(vals) {
              message("Vals[1]: ", vals[[1]])
              message("Vals[2]: ", vals[[2]])

              # cat(force)

              # Shut down the app so the telemetry can be seen easily
              if (vals[[1]] < 5) {
                updateSliderInput(
                  "x",
                  value = vals[[1]] + 1,
                  session = session
                )
              } else {
                shutdown()
              }
            })
          })

          # |>
          # bindOtel()

          # Set the value late in the reactive calc
          observe(label = "set_x", {
            message("Setting x!")
            xVal(input$x)
          })
        })
      }
      xMod("mymod")
    }
  )

  withr::with_options(
    list(
      shiny.otel.bind = "all"
    ),
    {
      runApp(app)
    }
  )
}


# ------------------------------------------

utils::globalVariables(".GenericCallEnv", add = TRUE)


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
#'     Sys.sleep(2) # Pretend this is expensive
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
#' }
#'
#' ## Only run app examples in interactive R sessions
#' if (interactive()) {
#'   # Basic example
#'   shinyApp(
#'     ui = fluidPage(
#'       sliderInput("x", "x", 1, 10, 5),
#'       sliderInput("y", "y", 1, 10, 5),
#'       div("x * y: "),
#'       verbatimTextOutput("txt")
#'     ),
#'     server = function(input, output) {
#'       r <- reactive({
#'         # The value expression is an _expensive_ computation
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }) %>%
#'         bindCache(input$x, input$y)
#'
#'       output$txt <- renderText(r())
#'     }
#'   )
#'
#'
#'   # Caching renderText
#'   shinyApp(
#'     ui = fluidPage(
#'       sliderInput("x", "x", 1, 10, 5),
#'       sliderInput("y", "y", 1, 10, 5),
#'       div("x * y: "),
#'       verbatimTextOutput("txt")
#'     ),
#'     server = function(input, output) {
#'       output$txt <- renderText({
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }) %>%
#'         bindCache(input$x, input$y)
#'     }
#'   )
#'
#'
#'   # Demo of using events and caching with an actionButton
#'   shinyApp(
#'     ui = fluidPage(
#'       sliderInput("x", "x", 1, 10, 5),
#'       sliderInput("y", "y", 1, 10, 5),
#'       actionButton("go", "Go"),
#'       div("x * y: "),
#'       verbatimTextOutput("txt")
#'     ),
#'     server = function(input, output) {
#'       r <- reactive({
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }) %>%
#'         bindCache(input$x, input$y) %>%
#'         bindEvent(input$go)
#'       # The cached, eventified reactive takes a reactive dependency on
#'       # input$go, but doesn't use it for the cache key. It uses input$x and
#'       # input$y for the cache key, but doesn't take a reactive dependency on
#'       # them, because the reactive dependency is superseded by addEvent().
#'
#'       output$txt <- renderText(r())
#'     }
#'   )
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
  # Set flag for otel logging when setting the value
  impl$.isLoggingOtel <- TRUE

  class(x) <- c("reactiveVal.otel", class(x))

  x
}

#' @export
bindOtel.reactivevalues <- function(x, ...) {
  rlang::check_dots_empty()

  impl <- attr(x, ".impl", exact = TRUE)
  # Set flag for otel logging when setting values
  impl$.isLoggingOtel <- TRUE

  class(x) <- c("reactivevalues.otel", class(x))

  x
}

#' @export
bindOtel.reactiveExpr <- function(x, ...) {
  rlang::check_dots_empty()

  domain <- reactive_get_domain(x)

  x_label <- attr(x, "observable", exact = TRUE)[[".label"]]
  span_label <- ospan_label_reactive(x, domain = domain)

  valueFunc <- reactive_get_value_func(x)
  origFunc <- valueFunc
  ospan_attrs <- otel_srcref_attributes(origFunc)
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
  if (
    exists(".GenericCallEnv") &&
      exists(".", envir = .GenericCallEnv, inherits = FALSE)
  ) {
    rm(list = ".", envir = .GenericCallEnv, inherits = FALSE)
  }

  # Turn off binding all otel, so that we don't recursively bind forever
  # `withOtel()` does not virally enable/disable binding all otel,
  # only in "this" tick
  withOtel(bind = "none", {
    res <- reactive(label = x_label, domain = domain, {
      # Force all `{shiny}` spans to be under `{shiny}` tracer, not the app's tracer
      # with_shiny_ospan_async(span_label, {
      with_ospan_async(
        span_label,
        {
          # TODO: Need `otel::with_tracer(tracer, CODE)`
          # Set the app's tracer to be the default when running the expression
          # with_app_tracer(domain = domain, {
          valueFunc()
          # })
        },
        attributes = ospan_attrs,
        domain = domain
      )
    })
  })

  # Set the original function as an attribute for later introspection
  internal_observable <- attr(res, "observable", exact = TRUE)
  internal_observable$.origFunc <- origFunc

  class(res) <- c("reactive.otel", class(res))
  res
}

#' @export
bindOtel.shiny.render.function <- function(x, ...) {
  rlang::check_dots_empty()

  valueFunc <- x
  span_label <- NULL
  ospan_attrs <- attr(x, "otelAttrs")

  renderFunc <- function(...) {
    session <- getDefaultReactiveDomain()
    if (is.null(span_label)) {
      span_label <<- ospan_label_render_function(domain = session)
    }

    with_ospan_async(
      span_label,
      {
        valueFunc(...)
      },
      attributes = ospan_attrs,
      domain = session
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
bindOtel.Observer <- function(x, ...) {
  rlang::check_dots_empty()

  if (x$.execCount > 0) {
    stop(
      "Cannot call bindOtel() on an Observer that has already been executed."
    )
  }

  obsFunc <- x$.func
  origFunc <- attr(x$.func, "wrappedFunc")
  ospan_attrs <- x$.otelAttrs

  domain <- x$.domain

  span_label <- ospan_label_observer(x, domain = domain)

  x$.func <- wrapFunctionLabel(
    name = span_label,
    ..stacktraceon = FALSE,
    func = function() {
      with_ospan_async(
        span_label,
        {
          force(obsFunc())
        },
        domain = domain,
        attributes = ospan_attrs
      )
    }
  )
  # attr(x$.func, "wrappedFunc") <- origFunc

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
bindOtel.reactivevalues.otel <- bindOtel.reactive.otel
#' @export
bindOtel.shiny.render.function.otel <- bindOtel.reactive.otel
#' @export
bindOtel.shiny.render.function.event <- bindOtel.reactive.event
#' @export
bindOtel.Observer.otel <- bindOtel.reactive.otel
