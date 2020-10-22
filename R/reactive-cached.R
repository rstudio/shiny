#' Create a reactive expression with caching
#'
#' @description
#'
#' Ordinary [reactive()] expressions will cache their most recent value. This
#' can make computation more efficient, because time-consuming code can execute
#' once and the result can be used in multiple different places without needing
#' to re-execute the code.
#'
#' With `cachedReactive()`, any number of previous values can be cached. The
#' most important difference in using `cachedReactive()` is that you must
#' provide `cacheKeyExpr`, which is an expression that generates as a _cache
#' key_, in addition to `valueExpr`, which is an expression that generates the
#' value returned by the `cachedReactive()`.
#'
#' For each possible value returned by `cacheKeyExpr`, there should be one
#' possible value returned by `valueExpr`; a given value of `cacheKeyExpr`
#' should not correspond to multiple possible values of `valueExpr`.
#'
#' The way to use this is to have a `cacheKeyExpr` that is fast to compute, and
#' a `valueExpr` that is expensive to compute. It evaluates the `cacheKeyExpr`,
#' and then serializes and hashes the result. If the resulting hashed key is in
#' the cache, then it simply retrieves and returns it; if not, then it executes
#' `valueExpr`, stores the result in the cache, and returns it.
#'
#' The `cacheKeyExpr` should be fast to compute, and ideally it will return an
#' object that is not too large, since the object must be serialized and hashed.
#' Typically, `cacheKeyExpr` will use any reactive inputs that are used by
#' `valueExpr`, but will do something that is fast and simple with them, like
#' put them together in a list. It is also best to use non-reference objects,
#' since the serialization of these objects may not capture relevant changes.
#'
#' `cachedReactive()` also can take an `eventExpr` so that it behaves similar to
#' [eventReactive()]. See more in the **Event expression** section.
#'
#' @section Cache keys and reactivity:
#'
#'   Because the value from `valueExpr` is cached, it is not necessarily
#'   re-executed, and therefore it can't be used to decide what objects to take
#'   reactive dependencies on. Instead, the `cacheKeyExpr` is used to figure out
#'   which objects to take reactive dependencies on. In short, the
#'   `cacheKeyExpr` is reactive, and `valueExpr` is not (it is run inside of
#'   [isolate()]).
#'
#'   Here's an example of what not to do: if `cacheKeyExpr=input$x` and
#'   `valueExpr={input$x + input$y}`, then it will only take a reactive
#'   dependency on `input$x` -- it won't recompute `valueExpr` when `input$y`
#'   changes. Moreover, the cache won't use `input$y` as part of the key, and so
#'   it could return incorrect values in the future when it retrieves values
#'   from the cache. (See the examples below for an example of this.)
#'
#'   A better cache key would be something like `list(input$x, input$y)`. This
#'   does two things: it ensures that a reactive dependency is taken on both `x`
#'   and `y`, and it also makes sure that both values are represented in the
#'   cache key.
#'
#'   In general, `cacheKeyExpr` should use the same reactive inputs as
#'   `valueExpr`, but the computation should be simpler. If there are other
#'   (non-reactive) values that are consumed, such as external data sources,
#'   they should be used in the `cacheKeyExpr` as well. Note that if the
#'   `cacheKeyExpr` is large, it can make sense to do some sort of reduction on
#'   it so that the serialization and hashing of the cache key is not too
#'   expensive.
#'
#'   Remember that the `cacheKeyExpr` is _reactive_, so it is not re-executed
#'   every single time that someone accesses the `cachedReactive`. For example,
#'   suppose we have this `cachedReactive`:
#'
#'   ```
#'   r <- cachedReactive(
#'     cacheKeyExpr = list(input$x, input$y),
#'     valueExpr = { input$x + input$y }
#'   )
#'   ```
#'
#' The first time someone calls `r()`, it executes both `cacheKeyExpr` and
#' `valueExpr`. If someone calls `r()` again, then it does not need to
#' re-execute `cacheKeyExpr`, because that `expr` has not been invalidated
#' via a change to `input$x` or `input$y`; it simply returns the previous value.
#' However, if `input$x` or `input$y` changes, then the reactive expression will
#' be invalidated, and the next time that someone calls `r()`, `cacheKeyExpr`
#' will need to be re-executed.
#'
#'
#' @section Event expressions and reactivity:
#'
#'   Typically, the `cacheKeyExpr` is reactive, and the `valueExpr` is not (it
#'   is run within [isolate()]). However, there are times when this isn't ideal:
#'   perhaps you have [sliderInput]s `x` and `y`, but you don't want the
#'   computation to occur until the user sets both `x` and `y`, and then clicks
#'   on an [actionButton] named `go`.
#'
#'   The value of `input$go` shouldn't be included in the cache key, because its
#'   value is not relevant to the calculation involving `input$x` and `input$y`.
#'   You also don't want to take a reactive dependency on `input$x` and
#'   `input$y`, because then any changes to those values would cause this cached
#'   reactive to do the computation or fetch the value from the cache and return
#'   it.
#'
#'   In short, in this case, you want to take a reactive dependency on
#'   `input$go` and not use its value for the cache key, and you want to **not**
#'   take a reactive dependency on `input$x` and `input$y` but use those values
#'   for the cache key.
#'
#'   This can be done by using `eventExpr`, similar to in the [eventReactive()]
#'   function. If a non-NULL `eventExpr` is provided to `cachedReactive()`, then
#'   it will be used for the reactive dependencies but its value will be
#'   ignored, and the `cacheKeyExpr` will be used for its value, but it will be
#'   executed in an [isolate()], so it will not be used for its reactive
#'   dependencies.
#'
#'   In the example described above, you would use something like:
#'
#'   ```
#'   r <- cachedReactive(
#'     eventExpr = input$go,
#'     cacheKeyExpr = list(input$x, input$y),
#'     valueExpr = { input$x + input$y }
#'   )
#'   ```
#'
#'   @section Async with cached reactives:
#'
#'   With a cached reactive expression, they key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises -- not regular R
#'   promises, but rather objects provided by the promises package, which are
#'   similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.)
#'
#'   If `valueExpr` is a promise, then anything that consumes the
#'   `cachedReactive` must expect it to return a promise.
#'
#'   Similarly, if `cacheKeyExpr` or `eventExpr` is a promise (in other words,
#'   if they are asynchronous), then the entire `cachedReactive` must be
#'   asynchronous, since the key must be computed before `valueExpr` is
#'   evaluated (or the value is retrieved from the cache). Anything that
#'   consumes the `cachedReactive` must therefore expect it to return a promise.
#'
#' @inheritParams reactive
#' @param cacheKeyExpr An expression or quosure that returns a value that will
#'   be hashed and used as a cache key. This key should be a unique identifier
#'   for the value: the assumption is that if the cache key is the same, then
#'   the value of `expr` is the same. If two separate `cachedReactive`s have the
#'   same key, the value is assumed to be the same. To avoid this, you can add
#'   an arbitrary identifier (like an ID string) to the cache key.
#' @param valueExpr The expression or quosure that produces the return value of
#'   the `cachedReactive`. It will be executed within an [isolate()] scope.
#' @param eventExpr An optional expression or quosure used for reactivity. If
#'   non-NULL, then `eventExpr` will be evaluated in a reactive context, and
#'   `valueExpr` will be evaluated in an isolated context (and it will not be
#'   used for reactive dependencies).
#' @param ignoreNULL If `TRUE`, then if `eventExpr` evaluates to `NULL`, then a
#'   silent exception will be raised and the cache key and the value will not be
#'   computed. See [req()] for more on silent exceptions.
#' @param cache The scope of the cache, or a cache object. This can be `"app"`
#'   (the default), `"session"`, or a cache object like a [diskCache()]. See the
#'   Cache Scoping section for more information.
#' @param ... Other arguments are ignored.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     "x * y: ",
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- cachedReactive(
#'       # Use input$x and input$y for the cache key. This is a reactive
#'       # expression.
#'       list(input$x, input$y),
#'       {
#'         # The value expression is an expensive computation
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }
#'     )
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#'
#' # Demo of using eventExpr with an actionButton
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     actionButton("go", "Go"),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- cachedReactive(
#'       # Take a reactive dependency on input$go, but don't use its value
#'       # for the cache key.
#'       eventExpr = input$go,
#'       # Use input$x and input$y for the cache key, but don't take a reactive
#'       # dependency on them.
#'       cacheKeyExpr = {
#'         list(input$x, input$y)
#'       },
#'       {
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }
#'     )
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#'
#' # Demo of cache key collisions and how to avoid them
#' shinyApp(
#'   ui = fluidPage(
#'     numericInput("n", "n", 1),
#'     "n * 2: ", verbatimTextOutput("txt2"),
#'     "n * 3: ", verbatimTextOutput("txt3"),
#'     "n * 4: ", verbatimTextOutput("txt4")
#'
#'   ),
#'   server = function(input, output) {
#'     r2 <- cachedReactive(input$n, {
#'       input$n * 2
#'     })
#'     output$txt2 <- renderText(r2())
#'
#'     # BAD: This uses the same cache key as r2 above, and so it will collide with
#'     # r2.
#'     r3 <- cachedReactive(input$n, {
#'       input$n * 3
#'     })
#'     output$txt3 <- renderText(r3())
#'
#'     # GOOD: By adding an ID string,this uses a different cache key from r2,
#'     # and so its keys will not collide with those from r2.
#'     r4 <- cachedReactive(list(input$n, "times4"), {
#'       input$n * 4
#'     })
#'     output$txt4 <- renderText(r4())
#'   }
#' )
#'
#' }
#'
#' @export
cachedReactive <- function(
  cacheKeyExpr,
  valueExpr,
  ...,
  eventExpr = NULL,
  ignoreNULL = TRUE,
  label = NULL,
  domain = getDefaultReactiveDomain(),
  cache = "app"
) {
  check_dots_empty()

  # eventExpr is optional
  eventExpr <- enquo(eventExpr)
  if (identical(get_expr(eventExpr), NULL)) {
    eventFunc <- NULL
  } else {
    eventFunc <- as_function(eventExpr)
  }

  label <- exprToLabel(substitute(cacheKeyExpr), "cachedReactive", label)

  cacheKeyFunc <- as_function(enquo(cacheKeyExpr))

  valueFunc <- as_function(enquo(valueExpr))
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveValueFunc", ..stacktraceon = TRUE)

  reactive(label = label, {
    # Set up the first steps in the hybrid chain. If there's no eventFunc, then
    # the first step is just the cacheKeyFunc(). If there is an eventFunc(),
    # then start with that, and isolate the cacheKeyFunc().
    if (is.null(eventFunc)) {
      firstStep <- cacheKeyFunc
    } else {
      firstStep <- hybrid_chain(
        eventFunc(),
        function(value) {
          req(!ignoreNULL || !isNullEvent(value))
          isolate(cacheKeyFunc())
        }
      )
    }

    # Connect the (maybe) eventExpr and cacheKeyExpr with the valueExpr
    hybrid_chain(
      firstStep(),
      function(cacheKeyResult) {
        cache <- resolve_cache_object(cache, domain)
        key   <- digest(cacheKeyResult, algo = "spookyhash")
        res <- cache$get(key)

        # Case 1: cache hit
        if (!is.key_missing(res)) {
          if (res$is_promise) {
            if (res$error) {
              return(promise_reject(valueWithVisible(res)))
            }
            return(promise_resolve(valueWithVisible(res)))

          } else {
            if (res$error) {
              stop(res$value)
            }
            return(valueWithVisible(res))
          }
        }

        # Case 2: cache miss
        #
        # valueFunc() might return a promise, or an actual value. Normally we'd
        # use a hybrid_chain() for this, but in this case, we need to have
        # different behavior if it's a promise or not a promise -- the
        # information about whether or not it's a promise needs to be stored in
        # the cache. We need to handle both cases and record in the cache
        # whether it's a promise or not, so that any consumer of the
        # cachedReactive() will be given the correct kind of object (a promise
        # vs. an actual value) in the case of a future cache hit.
        p <- withCallingHandlers(
          withVisible(isolate(valueFunc())),
          error = function(e) {
            cache$set(key, list(
              is_promise = FALSE,
              value      = e,
              visible    = TRUE,
              error      = TRUE
            ))
          }
        )

        if (is.promising(p$value)) {
            p$value <- p$value$
              then(function(value) {
                res <- withVisible(value)
                cache$set(key, list(
                  is_promise = TRUE,
                  value      = res$value,
                  visible    = res$visible,
                  error      = FALSE
                ))
                valueWithVisible(res)
              })$
              catch(function(e) {
                cache$set(key, list(
                  is_promise = TRUE,
                  value      = e,
                  visible    = TRUE,
                  error      = TRUE
                ))
                stop(e)
              })
            valueWithVisible(p)
        } else {
          # result is an ordinary value, not a promise.
          cache$set(key, list(
            is_promise = FALSE,
            value      = p$value,
            visible    = p$visible,
            error      = FALSE
          ))
          return(valueWithVisible(p))
        }
      })
    },
    domain = domain
  )
}
