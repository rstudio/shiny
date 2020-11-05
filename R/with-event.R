#' Make a reactive object respond only to specified events
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
withEvent <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
                      once = FALSE, label = NULL)
{
  check_dots_unnamed()
  force(ignoreNULL)
  force(ignoreInit)
  force(once)

  UseMethod("withEvent")
}


#' @export
withEvent.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}


#' @export
withEvent.reactiveExpr <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
  label = NULL)
{
  domain <- reactive_get_domain(x)

  eventFunc <- make_quos_func(enquos(...))

  valueFunc <- reactive_get_value_func(x)
  valueFunc <- wrapFunctionLabel(valueFunc, "eventReactiveValueFunc", ..stacktraceon = TRUE)

  if (is.null(label)) {
    label <- sprintf("eventReactive(%s)", paste(deparse(body(eventFunc)), collapse = "\n"))
  }

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)

  initialized <- FALSE

  res <- reactive(label = label, domain = domain, ..stacktraceon = FALSE, {
    hybrid_chain(
      eventFunc(),
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

  class(res) <- c("reactive.event", class(res))
  res
}


#' @export
withEvent.shiny.render.function <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE) {
  eventFunc <- make_quos_func(enquos(...))

  valueFunc <- x

  initialized <- FALSE

  res <- function() {
    hybrid_chain(
      eventFunc(),
      function(value) {
        if (ignoreInit && !initialized) {
          initialized <<- TRUE
          req(FALSE)
        }

        req(!ignoreNULL || !isNullEvent(value))

        isolate(valueFunc())
      }
    )
  }

  class(res) <- c("shiny.render.function.event", class(res))
  res
}


#' @export
withEvent.Observer <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE,
  once = FALSE, label = NULL)
{
  if (isTRUE(x$.destroyed)) {
    stop("Can't call withEvent() on an observer that has been destroyed.")
  }

  eventFunc <- make_quos_func(enquos(...))
  valueFunc <- x$.func

  if (is.null(label)) {
    label <- sprintf('observeEvent(%s)', paste(deparse(body(eventFunc)), collapse='\n'))
  }

  initialized <- FALSE

  res <- observe(
    label       = label,
    domain      = x$.domain,
    priority    = x$.priority,
    autoDestroy = x$.autoDestroy,
    suspended   = x$.suspended,
    ..stacktraceon = FALSE,
    {
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

  x$destroy()
  # Don't hold onto x, so that it can be gc'd
  rm(x)

  class(res) <- c("Observer.event", class(res))
  invisible(res)
}


#' @export
withEvent.reactive.event <-  function(x, ...) {
  stop("withEvent() has already been called on the object.")
}

#' @export
withEvent.Observer.event <- withEvent.reactive.event
