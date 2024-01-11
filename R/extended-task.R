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
#' @export
ExtendedTask <- R6Class("ExtendedTask", portable = TRUE,
  public = list(
    #' @description
    #' Creates a new `ExtendedTask` object. `ExtendedTask` should generally be
    #' created either at the top of a server function, or at the top of a module
    #' server function.
    #'
    #' @param func The long-running operation to execute. This should be an
    #'   asynchronous function, meaning, it should use the
    #'   [\{promises\}](https://rstudio.github.io/promises/) package, most
    #'   likely in conjuction with the
    #'   [\{future\}](https://rstudio.github.io/promises/articles/promises_04_futures.html)
    #'   package. (In short, the return value of `func` should be a
    #'   [`Future`][future::future()] object, or a `promise`, or something else
    #'   that [promises::as.promise()] understands.)
    #'
    #'   It's also important that this logic does not read from any
    #'   reactive inputs/sources, as inputs may change after the function is
    #'   invoked; instead, if the function needs to access reactive inputs, it
    #'   should take parameters and the caller of the `invoke()` method should
    #'   read reactive inputs and pass them as arguments.
    initialize = function(func) {
      private$func <- func
      private$rv_status <- reactiveVal("initial")
      private$rv_value <- reactiveVal(NULL)
      private$rv_error <- reactiveVal(NULL)
      private$invocation_queue <- fastmap::fastqueue()
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

      if (
        private$rv_status() == "running" ||
          private$invocation_queue$size() > 0
      ) {
        private$invocation_queue$add(args)
      } else {
        private$do_invoke(args)
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
        running = req(FALSE, cancelOutput="progress"),
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

    do_invoke = function(args) {
      private$rv_status("running")
      private$rv_value(NULL)
      private$rv_error(NULL)

      p <- NULL
      tryCatch({
        maskReactiveContext({
          # TODO: Bounce the do.call off of a promise_resolve(), so that the
          # call to invoke() always returns immediately?
          result <- do.call(private$func, args)
          p <- promises::as.promise(result)
        })
      }, error = function(e) {
        private$on_error(e)
      })

      promises::finally(
        promises::then(p,
          onFulfilled = function(value, .visible) {
            private$on_success(list(value=value, visible=.visible))
          },
          onRejected = function(error) {
            private$on_error(error)
          }
        ),
        onFinally = function() {
          if (private$invocation_queue$size() > 0) {
            private$do_invoke(private$invocation_queue$remove())
          }
        }
      )


      invisible(NULL)
    },

    on_error = function(err) {
      private$rv_status("error")
      private$rv_error(err)
    },

    on_success = function(value) {
      private$rv_status("success")
      private$rv_value(value)
    }
  )
)
