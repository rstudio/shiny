#' Event decorator
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
