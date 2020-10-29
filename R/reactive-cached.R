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
#' provide `key`, which is an expression that generates as a _cache key_, in
#' addition to `value`, which is an expression that generates the value returned
#' by the `cachedReactive()`.
#'
#' For each possible value returned by `key`, there should be one possible value
#' returned by `value`; a given value of `key` should not correspond to multiple
#' possible values of `value`.
#'
#' The way to use this is to have a `key` that is fast to compute, and a `value`
#' that is expensive to compute. It evaluates the `key`, and then serializes and
#' hashes the result. If the resulting hashed key is in the cache, then it
#' simply retrieves and returns it; if not, then it executes `value`, stores the
#' result in the cache, and returns it.
#'
#' The `key` should be fast to compute, and ideally it will return an object
#' that is not too large, since the object must be serialized and hashed.
#' Typically, `key` will use any reactive inputs that are used by `value`, but
#' will do something that is fast and simple with them, like put them together
#' in a list. It is also best to use non-reference objects, since the
#' serialization of these objects may not capture relevant changes.
#'
#' `cachedReactive()` also can take an `event` so that it behaves similar to
#' [eventReactive()]. See more in the **Event expression** section.
#'
#' @section Cache keys and reactivity:
#'
#'   Because the value from `value` is cached, it is not necessarily
#'   re-executed, and therefore it can't be used to decide what objects to take
#'   reactive dependencies on. Instead, the `key` is used to figure out which
#'   objects to take reactive dependencies on. In short, the `key` is reactive,
#'   and `value` is not (it is run inside of [isolate()]).
#'
#'   Here's an example of what not to do: if `key=input$x` and `value={input$x +
#'   input$y}`, then it will only take a reactive dependency on `input$x` -- it
#'   won't recompute `value` when `input$y` changes. Moreover, the cache won't
#'   use `input$y` as part of the key, and so it could return incorrect values
#'   in the future when it retrieves values from the cache. (See the examples
#'   below for an example of this.)
#'
#'   A better cache key would be something like `list(input$x, input$y)`. This
#'   does two things: it ensures that a reactive dependency is taken on both `x`
#'   and `y`, and it also makes sure that both values are represented in the
#'   cache key.
#'
#'   In general, `key` should use the same reactive inputs as `value`, but the
#'   computation should be simpler. If there are other (non-reactive) values
#'   that are consumed, such as external data sources, they should be used in
#'   the `key` as well. Note that if the `key` is large, it can make sense to do
#'   some sort of reduction on it so that the serialization and hashing of the
#'   cache key is not too expensive.
#'
#'   Remember that the `key` is _reactive_, so it is not re-executed every
#'   single time that someone accesses the `cachedReactive`. For example,
#'   suppose we have this `cachedReactive`:
#'
#'   ```
#'   r <- cachedReactive(
#'     key = list(input$x, input$y),
#'     value = { input$x + input$y }
#'   )
#'   ```
#'
#' The first time someone calls `r()`, it executes both `key` and `value`. If
#' someone calls `r()` again, then it does not need to re-execute `key`, because
#' that `expr` has not been invalidated via a change to `input$x` or `input$y`;
#' it simply returns the previous value. However, if `input$x` or `input$y`
#' changes, then the reactive expression will be invalidated, and the next time
#' that someone calls `r()`, `key` will need to be re-executed.
#'
#'
#' @section Event expressions and reactivity:
#'
#'   Typically, the `key` is reactive, and the `value` is not (it is run within
#'   [isolate()]). However, there are times when this isn't ideal: perhaps you
#'   have [sliderInput]s `x` and `y`, but you don't want the computation to
#'   occur until the user sets both `x` and `y`, and then clicks on an
#'   [actionButton] named `go`.
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
#'   This can be done by using `event`, similar to in the [eventReactive()]
#'   function. If a non-NULL `event` is provided to `cachedReactive()`, then it
#'   will be used for the reactive dependencies but its value will be ignored,
#'   and the `key` will be used for its value, but it will be executed in an
#'   [isolate()], so it will not be used for its reactive dependencies.
#'
#'   In the example described above, you would use something like:
#'
#'   ```
#'   r <- cachedReactive(
#'     event = input$go,
#'     key = list(input$x, input$y),
#'     value = { input$x + input$y }
#'   )
#'   ```
#'
#' @section Async with cached reactives:
#'
#'   With a cached reactive expression, they key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises -- not regular R
#'   promises, but rather objects provided by the promises package, which are
#'   similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.) You can also use [future::future()] objects to run code in a
#'   separate process or even on a remote machine.
#'
#'   If `value` is a promise, then anything that consumes the `cachedReactive`
#'   must expect it to return a promise.
#'
#'   Similarly, if `key` or `event` is a promise (in other words, if they are
#'   asynchronous), then the entire `cachedReactive` must be asynchronous, since
#'   the key must be computed before `value` is evaluated (or the value is
#'   retrieved from the cache). Anything that consumes the `cachedReactive` must
#'   therefore expect it to return a promise.
#'
#' @inheritParams reactive
#' @param key An expression or quosure that returns a value that will be hashed
#'   and used as a cache key. This key should be a unique identifier for the
#'   value: the assumption is that if the cache key is the same, then the value
#'   of `expr` is the same. If two separate `cachedReactive`s have the same key,
#'   the value is assumed to be the same. To avoid this, you can add an
#'   arbitrary identifier (like an ID string) to the cache key.
#' @param value The expression or quosure that produces the return value of the
#'   `cachedReactive`. It will be executed within an [isolate()] scope.
#' @param event An optional expression or quosure used for reactivity. If
#'   non-NULL, then `event` will be evaluated in a reactive context, and `value`
#'   will be evaluated in an isolated context (and it will not be used for
#'   reactive dependencies).
#' @param ignoreNULL If `TRUE`, then if `event` evaluates to `NULL`, then a
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
#' # Demo of using event with an actionButton
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
#'       event = input$go,
#'       # Use input$x and input$y for the cache key, but don't take a reactive
#'       # dependency on them.
#'       key = {
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
  key,
  value,
  ...,
  event = NULL,
  ignoreNULL = TRUE,
  label = NULL,
  domain = getDefaultReactiveDomain(),
  cache = "app"
) {
  check_dots_empty()

  # event is optional
  event <- enquo(event)
  if (identical(get_expr(event), NULL)) {
    eventFunc <- NULL
  } else {
    eventFunc <- as_function(event)
  }

  label <- exprToLabel(substitute(key), "cachedReactive", label)

  keyFunc <- as_function(enquo(key))

  valueFunc <- as_function(enquo(value))
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveValueFunc", ..stacktraceon = TRUE)

  # Hash the value expression now -- this will be added to the key later on, to
  # reduce the chance of key collisions with other cachedReactives. Remove
  # source refs because they can differ even though the code is the same.
  valueExprHash <- digest(removeSource(get_expr(enquo(value))), algo = "spookyhash")
  message("valuExprHash: ", valueExprHash)

  reactive(label = label, {
    # Set up the first steps in the hybrid chain. If there's no eventFunc, then
    # the first step is just the keyFunc(). If there is an eventFunc(),
    # then start with that, and isolate the keyFunc().
    if (is.null(eventFunc)) {
      firstStep <- keyFunc
    } else {
      firstStep <- hybrid_chain(
        eventFunc(),
        function(value) {
          req(!ignoreNULL || !isNullEvent(value))
          isolate(keyFunc())
        }
      )
    }

    # Connect the (maybe) event and key with the value
    hybrid_chain(
      firstStep(),
      function(cacheKeyResult) {
        cache <- resolve_cache_object(cache, domain)
        key_str <- digest(list(cacheKeyResult, valueExprHash), algo = "spookyhash")
        res <- cache$get(key_str)

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
            cache$set(key_str, list(
              is_promise = FALSE,
              value      = e,
              visible    = TRUE,
              error      = TRUE
            ))
          }
        )

        if (is.promising(p$value)) {
          p$value <- as.promise(p$value)
          p$value <- p$value$
            then(function(value) {
              res <- withVisible(value)
              cache$set(key_str, list(
                is_promise = TRUE,
                value      = res$value,
                visible    = res$visible,
                error      = FALSE
              ))
              valueWithVisible(res)
            })$
            catch(function(e) {
              cache$set(key_str, list(
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
          cache$set(key_str, list(
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
