#' Event decorator
#' @export
withEvent <- function(x, event = NULL, ignoreNULL = TRUE, ignoreInit = FALSE) {
  UseMethod("withEvent")
}


#' @export
withEvent.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}


#' @export
withEvent.reactive <- function(x, event, ignoreNULL = TRUE, ignoreInit = FALSE) {
  label <- exprToLabel(substitute(event), "eventReactive")
  domain <- reactive_get_domain(x)

  eventFunc <- as_function(enquo(event))

  valueFunc <- reactive_get_value_func(x)
  valueFunc <- wrapFunctionLabel(valueFunc, "eventReactiveValueFunc", ..stacktraceon = TRUE)

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
withEvent.shiny.render.function <- function(x, event, ignoreNULL = TRUE, ignoreInit = FALSE) {
  eventFunc <- as_function(enquo(event))

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
