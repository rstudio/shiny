#' Make an object respond only to specified reactive events
#'
#' @description
#'
#' Modify an object to respond to "event-like" reactive inputs, values, and
#' expressions. `bindEvent()` can be used with reactive expressions, render
#' functions, and observers. The resulting object takes a reactive dependency on
#' the `...` arguments, and not on the original object's code. This can, for
#' example, be used to make an observer execute only when a button is pressed.
#'
#' `bindEvent()` was added in Shiny 1.6.0. When it is used with [reactive()] and
#' [observe()], it does the same thing as [eventReactive()] and
#' [observeEvent()]. However, `bindEvent()` is more flexible: it can be combined
#' with [bindCache()], and it can also be used with `render` functions (like
#' [renderText()] and [renderPlot()]).
#'
#' @section Details:
#'
#'   Shiny's reactive programming framework is primarily designed for calculated
#'   values (reactive expressions) and side-effect-causing actions (observers)
#'   that respond to *any* of their inputs changing. That's often what is
#'   desired in Shiny apps, but not always: sometimes you want to wait for a
#'   specific action to be taken from the user, like clicking an
#'   [actionButton()], before calculating an expression or taking an action. A
#'   reactive value or expression that is used to trigger other calculations in
#'   this way is called an *event*.
#'
#'   These situations demand a more imperative, "event handling" style of
#'   programming that is possible--but not particularly intuitive--using the
#'   reactive programming primitives [observe()] and [isolate()]. `bindEvent()`
#'   provides a straightforward API for event handling that wraps `observe` and
#'   `isolate`.
#'
#'   The `...` arguments are captured as expressions and combined into an
#'   **event expression**. When this event expression is invalidated (when its
#'   upstream reactive inputs change), that is an **event**, and it will cause
#'   the original object's code to execute.
#'
#'   Use `bindEvent()` with `observe()` whenever you want to *perform an action*
#'   in response to an event. (This does the same thing as [observeEvent()],
#'   which was available in Shiny prior to version 1.6.0.) Note that
#'   "recalculate a value" does not generally count as performing an action --
#'   use [reactive()] for that.
#'
#'   Use `bindEvent()` with `reactive()` to create a *calculated value* that
#'   only updates in response to an event. This is just like a normal [reactive
#'   expression][reactive] except it ignores all the usual invalidations that
#'   come from its reactive dependencies; it only invalidates in response to the
#'   given event. (This does the same thing as [eventReactive()], which was
#'   available in Shiny prior to version 1.6.0.)
#'
#'   `bindEvent()` is often used with [bindCache()].
#'
#' @section ignoreNULL and ignoreInit:
#'
#'   `bindEvent()` takes an `ignoreNULL` parameter that affects behavior when
#'   the event expression evaluates to `NULL` (or in the special case of an
#'   [actionButton()], `0`). In these cases, if `ignoreNULL` is `TRUE`, then it
#'   will raise a silent [validation][validate] error. This is useful behavior
#'   if you don't want to do the action or calculation when your app first
#'   starts, but wait for the user to initiate the action first (like a "Submit"
#'   button); whereas `ignoreNULL=FALSE` is desirable if you want to initially
#'   perform the action/calculation and just let the user re-initiate it (like a
#'   "Recalculate" button).
#'
#'   `bindEvent()` also takes an `ignoreInit` argument. By default, reactive
#'   expressions and observers will run on the first reactive flush after they
#'   are created (except if, at that moment, the event expression evaluates to
#'   `NULL` and `ignoreNULL` is `TRUE`). But when responding to a click of an
#'   action button, it may often be useful to set `ignoreInit` to `TRUE`. For
#'   example, if you're setting up an observer to respond to a dynamically
#'   created button, then `ignoreInit = TRUE` will guarantee that the action
#'   will only be triggered when the button is actually clicked, instead of also
#'   being triggered when it is created/initialized. Similarly, if you're
#'   setting up a reactive that responds to a dynamically created button used to
#'   refresh some data (which is then returned by that `reactive`), then you
#'   should use `reactive(...) %>% bindEvent(..., ignoreInit = TRUE)` if you
#'   want to let the user decide if/when they want to refresh the data (since,
#'   depending on the app, this may be a computationally expensive operation).
#'
#'   Even though `ignoreNULL` and `ignoreInit` can be used for similar purposes
#'   they are independent from one another. Here's the result of combining
#'   these:

#'
#' \describe{
#'   \item{`ignoreNULL = TRUE` and `ignoreInit = FALSE`}{
#'      This is the default. This combination means that reactive/observer code
#'       will run every time that event expression is not
#'      `NULL`. If, at the time of creation, the event expression happens
#'      to *not* be `NULL`, then the code runs.
#'   }
#'   \item{`ignoreNULL = FALSE` and `ignoreInit = FALSE`}{
#'      This combination means that reactive/observer code will
#'      run every time no matter what.
#'   }
#'   \item{`ignoreNULL = FALSE` and `ignoreInit = TRUE`}{
#'      This combination means that reactive/observer code will
#'      *not* run at the time of creation (because `ignoreInit = TRUE`),
#'      but it will run every other time.
#'   }
#'   \item{`ignoreNULL = TRUE` and `ignoreInit = TRUE`}{
#'      This combination means that reactive/observer code will
#'      *not* at the time of creation (because  `ignoreInit = TRUE`).
#'      After that, the reactive/observer code will run every time that
#'      the event expression is not `NULL`.
#'   }
#' }
#'

#' @section Types of objects:
#'
#'   `bindEvent()` can be used with reactive expressions, observers, and shiny
#'   render functions.
#'
#'   When `bindEvent()` is used with `reactive()`, it creates a new reactive
#'   expression object.
#'
#'   When `bindEvent()` is used with `observe()`, it alters the observer in
#'   place. It can only be used with observers which have not yet executed.
#'
#' @section Combining events and caching:
#'
#'   In many cases, it makes sense to use `bindEvent()` along with
#'   `bindCache()`, because they each can reduce the amount of work done on the
#'   server. For example, you could have [sliderInput]s `x` and `y` and a
#'   `reactive()` that performs a time-consuming operation with those values.
#'   Using `bindCache()` can speed things up, especially if there are multiple
#'   users. But it might make sense to also not do the computation until the
#'   user sets both `x` and `y`, and then clicks on an [actionButton] named
#'   `go`.
#'
#'   To use both caching and events, the object should first be passed to
#'   `bindCache()`, then `bindEvent()`. For example:

#'
#'   ```
#'   r <- reactive({
#'       Sys.sleep(2)  # Pretend this is an expensive computation
#'       input$x * input$y
#'     }) %>%
#'     bindCache(input$x, input$y) %>%
#'     bindEvent(input$go)
#'  ```

#'
#' Anything that consumes `r()` will take a reactive dependency on the event
#' expression given to `bindEvent()`, and not the cache key expression given to
#' `bindCache()`. In this case, it is just `input$go`.
#'
#' @param x An object to wrap so that is triggered only when a the specified
#'   event occurs.
#' @param ignoreNULL Whether the action should be triggered (or value
#'   calculated) when the input is `NULL`. See Details.
#' @param ignoreInit If `TRUE`, then, when the eventified object is first
#'   created/initialized, don't trigger the action or (compute the value). The
#'   default is `FALSE`. See Details.
#' @param once Used only for observers. Whether this `observer` should be
#'   immediately destroyed after the first time that the code in the observer is
#'   run. This pattern is useful when you want to subscribe to a event that
#'   should only happen once.
#' @param label A label for the observer or reactive, useful for debugging.
#' @param ... One or more expressions that represents the event; this can be a
#'   simple reactive value like `input$click`, a call to a reactive expression
#'   like `dataset()`, or even a complex expression inside curly braces. If
#'   there are multiple expressions in the `...`, then it will take a dependency
#'   on all of them.
#' @export
bindEvent <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
                      once = FALSE, label = NULL)
{
  check_dots_unnamed()
  force(ignoreNULL)
  force(ignoreInit)
  force(once)

  UseMethod("bindEvent")
}


#' @export
bindEvent.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}


#' @export
bindEvent.reactiveExpr <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
  label = NULL)
{
  domain <- reactive_get_domain(x)

  qs <- enquos0(...)
  eventFunc <- quos_to_func(qs)

  valueFunc <- reactive_get_value_func(x)
  valueFunc <- wrapFunctionLabel(valueFunc, "eventReactiveValueFunc", ..stacktraceon = TRUE)

  if (is.null(label)) {
    call_srcref <- attr(sys.call(-1), "srcref", exact = TRUE)
    label <- rassignSrcrefToLabel(
      call_srcref,
      defaultLabel = as_default_label(sprintf(
        'bindEvent(%s, %s)',
        attr(x, "observable", exact = TRUE)$.label,
        quos_to_label(qs)
      ))
    )
  }

  x_classes <- class(x)

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)

  initialized <- FALSE

  with_no_otel_bind({
    res <- reactive(label = label, domain = domain, ..stacktraceon = FALSE, {
      hybrid_chain(
        {
          eventFunc()
        },
        function(value) {
          if (ignoreInit && !initialized) {
            initialized <<- TRUE
            req(FALSE)
          }

          req(!ignoreNULL || !isNullEvent(value))

          isolate(valueFunc())
        }
      )
    })
  })

  class(res) <- c("reactive.event", x_classes)

  if (has_otel_bind("reactivity")) {
    res <- bind_otel_reactive_expr(res)
  }

  res
}


#' @export
bindEvent.shiny.render.function <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE) {
  eventFunc <- quos_to_func(enquos0(...))

  valueFunc <- x

  initialized <- FALSE

  renderFunc <- function(...) {
    hybrid_chain(
      eventFunc(),
      function(value) {
        if (ignoreInit && !initialized) {
          initialized <<- TRUE
          req(FALSE)
        }

        req(!ignoreNULL || !isNullEvent(value))

        isolate(valueFunc(...))
      }
    )
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- c("shiny.render.function.event", class(valueFunc))
  renderFunc
}


#' @export
bindEvent.Observer <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
  once = FALSE, label = NULL)
{
  if (x$.execCount > 0) {
    stop("Cannot call bindEvent() on an Observer that has already been executed.")
  }

  qs <- enquos0(...)
  eventFunc <- quos_to_func(qs)
  valueFunc <- x$.func

  # Note that because the observer will already have been logged by this point,
  # this updated label won't show up in the reactlog.
  if (is.null(label)) {
    call_srcref <- attr(sys.call(-1), "srcref", exact = TRUE)
    x$.label <- rassignSrcrefToLabel(
      call_srcref,
      defaultLabel = as_default_label(
        sprintf('bindEvent(%s, %s)', x$.label, quos_to_label(qs))
      )
    )
  } else {
    x$.label <- label
  }

  initialized <- FALSE

  x$.func <- wrapFunctionLabel(
    name = x$.label,
    ..stacktraceon = FALSE,
    func = function() {
      hybrid_chain(
        eventFunc(),
        function(value) {
          if (ignoreInit && !initialized) {
            initialized <<- TRUE
            return()
          }

          if (ignoreNULL && isNullEvent(value)) {
            return()
          }

          if (once) {
            on.exit(x$destroy())
          }

          req(!ignoreNULL || !isNullEvent(value))

          isolate(valueFunc())
        }
      )
    }
  )

  class(x) <- c("Observer.event", class(x))
  if (has_otel_bind("reactivity")) {
    x <- bind_otel_observe(x)
  }
  invisible(x)
}


#' @export
bindEvent.reactive.event <-  function(x, ...) {
  stop("bindEvent() has already been called on the object.")
}

#' @export
bindEvent.Observer.event <- bindEvent.reactive.event
