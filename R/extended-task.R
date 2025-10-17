#' Task or computation that proceeds in the background
#'
#' @description In normal Shiny reactive code, whenever an observer, calc, or
#'   output is busy computing, it blocks the current session from receiving any
#'   inputs or attempting to proceed with any other computation related to that
#'   session.
#'
#'   The `ExtendedTask` class allows you to have an expensive operation that is
#'   started by a reactive effect, and whose (eventual) results can be accessed
#'   by a regular observer, calc, or output; but during the course of the
#'   operation, the current session is completely unblocked, allowing the user
#'   to continue using the rest of the app while the operation proceeds in the
#'   background.
#'
#'   Note that each `ExtendedTask` object does not represent a _single
#'   invocation_ of its long-running function. Rather, it's an object that is
#'   used to invoke the function with different arguments, keeps track of
#'   whether an invocation is in progress, and provides ways to get at the
#'   current status or results of the operation. A single `ExtendedTask` object
#'   does not permit overlapping invocations: if the `invoke()` method is called
#'   before the previous `invoke()` is completed, the new invocation will not
#'   begin until the previous invocation has completed.
#'
#' @section `ExtendedTask` versus asynchronous reactives:
#'
#'   Shiny has long supported [using
#'   \{promises\}](https://rstudio.github.io/promises/articles/promises_06_shiny.html)
#'   to write asynchronous observers, calcs, or outputs. You may be wondering
#'   what the differences are between those techniques and this class.
#'
#'   Asynchronous observers, calcs, and outputs are not--and have never
#'   been--designed to let a user start a long-running operation, while keeping
#'   that very same (browser) session responsive to other interactions. Instead,
#'   they unblock other sessions, so you can take a long-running operation that
#'   would normally bring the entire R process to a halt and limit the blocking
#'   to just the session that started the operation. (For more details, see the
#'   section on ["The Flush
#'   Cycle"](https://rstudio.github.io/promises/articles/promises_06_shiny.html#the-flush-cycle).)
#'
#'   `ExtendedTask`, on the other hand, invokes an asynchronous function (that
#'   is, a function that quickly returns a promise) and allows even that very
#'   session to immediately unblock and carry on with other user interactions.
#'
#' @examplesIf rlang::is_interactive() && rlang::is_installed("mirai")
#' library(shiny)
#' library(bslib)
#' library(mirai)
#'
#' # Set background processes for running tasks
#' daemons(1)
#' # Reset when the app is stopped
#' onStop(function() daemons(0))
#'
#' ui <- page_fluid(
#'   titlePanel("Extended Task Demo"),
#'   p(
#'     'Click the button below to perform a "calculation"',
#'     "that takes a while to perform."
#'   ),
#'   input_task_button("recalculate", "Recalculate"),
#'   p(textOutput("result"))
#' )
#'
#' server <- function(input, output) {
#'   rand_task <- ExtendedTask$new(function() {
#'     mirai(
#'       {
#'         # Slow operation goes here
#'         Sys.sleep(2)
#'         sample(1:100, 1)
#'       }
#'     )
#'   })
#'
#'   # Make button state reflect task.
#'   # If using R >=4.1, you can do this instead:
#'   # rand_task <- ExtendedTask$new(...) |> bind_task_button("recalculate")
#'   bind_task_button(rand_task, "recalculate")
#'
#'   observeEvent(input$recalculate, {
#'     # Invoke the extended in an observer
#'     rand_task$invoke()
#'   })
#'
#'   output$result <- renderText({
#'     # React to updated results when the task completes
#'     number <- rand_task$result()
#'     paste0("Your number is ", number, ".")
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
ExtendedTask <- R6Class("ExtendedTask", portable = TRUE, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new `ExtendedTask` object. `ExtendedTask` should generally be
    #' created either at the top of a server function, or at the top of a module
    #' server function.
    #'
    #' @param func The long-running operation to execute. This should be an
    #'   asynchronous function, meaning, it should use the
    #'   [\{promises\}](https://rstudio.github.io/promises/) package, most
    #'   likely in conjunction with the
    #'   [\{mirai\}](https://mirai.r-lib.org) or
    #'   [\{future\}](https://rstudio.github.io/promises/articles/promises_04_futures.html)
    #'   package. (In short, the return value of `func` should be a
    #'   [`mirai`][mirai::mirai()], [`Future`][future::future()], `promise`,
    #'   or something else that [promises::as.promise()] understands.)
    #'
    #'   It's also important that this logic does not read from any
    #'   reactive inputs/sources, as inputs may change after the function is
    #'   invoked; instead, if the function needs to access reactive inputs, it
    #'   should take parameters and the caller of the `invoke()` method should
    #'   read reactive inputs and pass them as arguments.
    initialize = function(func) {
      private$func <- func

      # Do not show these private reactive values in otel spans
      with_no_otel_bind({
        private$rv_status <- reactiveVal("initial")
        private$rv_value <- reactiveVal(NULL)
        private$rv_error <- reactiveVal(NULL)
      })

      private$invocation_queue <- fastmap::fastqueue()

      domain <- getDefaultReactiveDomain()

      # Set a label for the reactive values for easier debugging
      # Go up an extra sys.call() to get the user's call to ExtendedTask$new()
      # The first sys.call() is to `initialize(...)`
      call_srcref <- attr(sys.call(-1), "srcref", exact = TRUE)
      label <- rassignSrcrefToLabel(
        call_srcref,
        defaultLabel = "<anonymous>",
        fnName = "ExtendedTask\\$new"
      )
      private$otel_label <- otel_label_extended_task(label, domain = domain)
      private$otel_label_add_to_queue <- otel_label_extended_task_add_to_queue(label, domain = domain)

      call_srcref <- attr(sys.call(-1), "srcref", exact = TRUE)
      private$otel_attrs <- c(
        otel_srcref_attributes(call_srcref),
        otel_session_id_attrs(domain)
      ) %||% list()

      set_rv_label <- function(rv, suffix) {
        impl <- attr(rv, ".impl", exact = TRUE)
        impl$.otelLabel <- otel_label_extended_task_set_reactive_val(
          label,
          suffix,
          domain = domain
        )
      }
      set_rv_label(private$rv_status, "status")
      set_rv_label(private$rv_value, "value")
      set_rv_label(private$rv_error, "error")
    },
    #' @description
    #' Starts executing the long-running operation. If this `ExtendedTask` is
    #' already running (meaning, a previous call to `invoke()` is not yet
    #' complete) then enqueues this invocation until after the current
    #' invocation, and any already-enqueued invocation, completes.
    #'
    #' @param ... Parameters to use for this invocation of the underlying
    #'   function. If reactive inputs are needed by the underlying function,
    #'   they should be read by the caller of `invoke` and passed in as
    #'   arguments.
    invoke = function(...) {
      args <- rlang::dots_list(..., .ignore_empty = "none")
      call <- rlang::caller_call(n = 0)

      if (
        isolate(private$rv_status()) == "running" ||
          private$invocation_queue$size() > 0
      ) {
        otel_log(
          private$otel_label_add_to_queue,
          severity = "debug",
          attributes = c(
            private$otel_attrs,
            list(
              queue_size = private$invocation_queue$size() + 1L
            )
          )
        )
        private$invocation_queue$add(list(args = args, call = call))
      } else {

        if (has_otel_bind("reactivity")) {
          private$ospan <- create_shiny_ospan(
            private$otel_label,
            attributes = private$otel_attrs
          )
          otel::local_active_span(private$ospan)
        }

        private$do_invoke(args, call = call)
      }
      invisible(NULL)
    },
    #' @description
    #' This is a reactive read that invalidates the caller when the task's
    #' status changes.
    #'
    #' Returns one of the following values:
    #'
    #' * `"initial"`: This `ExtendedTask` has not yet been invoked
    #' * `"running"`: An invocation is currently running
    #' * `"success"`: An invocation completed successfully, and a value can be
    #'   retrieved via the `result()` method
    #' * `"error"`: An invocation completed with an error, which will be
    #'   re-thrown if you call the `result()` method
    status = function() {
      private$rv_status()
    },
    #' @description
    #' Attempts to read the results of the most recent invocation. This is a
    #' reactive read that invalidates as the task's status changes.
    #'
    #' The actual behavior differs greatly depending on the current status of
    #' the task:
    #'
    #' * `"initial"`: Throws a silent error (like [`req(FALSE)`][req()]). If
    #'   this happens during output rendering, the output will be blanked out.
    #' * `"running"`: Throws a special silent error that, if it happens during
    #'   output rendering, makes the output appear "in progress" until further
    #'   notice.
    #' * `"success"`: Returns the return value of the most recent invocation.
    #' * `"error"`: Throws whatever error was thrown by the most recent
    #'   invocation.
    #'
    #' This method is intended to be called fairly naively by any output or
    #' reactive expression that cares about the output--you just have to be
    #' aware that if the result isn't ready for whatever reason, processing will
    #' stop in much the same way as `req(FALSE)` does, but when the result is
    #' ready you'll get invalidated, and when you run again the result should be
    #' there.
    #'
    #' Note that the `result()` method is generally not meant to be used with
    #' [observeEvent()], [eventReactive()], [bindEvent()], or [isolate()] as the
    #' invalidation will be ignored.
    result = function() {
      switch (private$rv_status(),
        running = req(FALSE, cancelOutput = "progress"),
        success = if (private$rv_value()$visible) {
          private$rv_value()$value
        } else {
          invisible(private$rv_value()$value)
        },
        error = stop(private$rv_error()),
        # default case (initial, cancelled)
        req(FALSE)
      )
    }
  ),
  private = list(
    func = NULL,
    # reactive value with "initial"|"running"|"success"|"error"
    rv_status = NULL,
    rv_value = NULL,
    rv_error = NULL,
    invocation_queue = NULL,

    otel_label = NULL,
    otel_attrs = list(),
    otel_label_add_to_queue = NULL,
    ospan = NULL,

    do_invoke = function(args, call = NULL) {
      private$rv_status("running")
      private$rv_value(NULL)
      private$rv_error(NULL)

      p <- promises::promise_resolve(
        maskReactiveContext(do.call(private$func, args))
      )

      p <- promises::then(
        p,
        onFulfilled = function(value, .visible) {
          if (is_ospan(private$ospan)) {
            private$ospan$end(status_code = "ok")
            private$ospan <- NULL
          }
          private$on_success(list(value = value, visible = .visible))
        },
        onRejected = function(error) {
          if (is_ospan(private$ospan)) {
            private$ospan$end(status_code = "error")
            private$ospan <- NULL
          }
          private$on_error(error, call = call)
        }
      )

      promises::finally(p, onFinally = function() {
        if (private$invocation_queue$size() > 0) {
          next_call <- private$invocation_queue$remove()
          private$do_invoke(next_call$args, next_call$call)
        }
      })

      invisible(NULL)
    },

    on_error = function(err, call = NULL) {
      cli::cli_warn(
        "ERROR: An error occurred when invoking the ExtendedTask.",
        parent = err,
        call = call
      )
      private$rv_status("error")
      private$rv_error(err)
    },

    on_success = function(value) {
      private$rv_status("success")
      private$rv_value(value)
    }
  )
)
