utils::globalVariables(".GenericCallEnv", add = TRUE)

#' Add caching with reactivity to an object
#'
#' @description
#'
#' `withCache()` adds caching to the following kinds of objects used in Shiny:
#'
#' * [reactive()]` expressions.
#' * `render*` functions, like [renderText()], [renderTable()], and so on.
#'
#' It is often used in conjunction with [withEvent()].
#'
#'
#' Ordinary [reactive()] expressions will cache their most recent value. This
#' can make computation more efficient, because time-consuming code can execute
#' once and the result can be used in multiple different places without needing
#' to re-execute the code.
#'
#' When `withCache()` is used on a reactive expression, any number of previous
#' values can be cached. You must provide one or more expressions that are used
#' to generate a _cache key_. The `...` expressions are put together in a list
#' and hashed, and the result is used as the cache **key**.
#'
#' There is also a **value** expression, which is the expression passed to the
#' original reactive.
#'
#' For each possible **key**, there should be one possible **value** returned by
#' the original reactive expression; a given key should not correspond to
#' multiple possible values from the reactive expression.
#'
#' To use `withCache(), the key should be fast to compute, and the value should
#' expensive (so that it benefits from caching).  To see the value should be
#' computed, a cached reactive evaluates the key, and then serializes and hashes
#' the result. If the resulting hashed key is in the cache, then the cached
#' reactive simply retrieves the previously calculated value and returns it; if
#' not, then the value is computed and the result is stored in the cache before
#' being returned.
#'
#' The key should be fast to compute, and ideally it will return an object that
#' is not too large, so that serializing and hashing the object is not
#' expensive.
#'
#' In most cases, the key will contain any reactive inputs that are used by the
#' value expression. It is also best to use non-reference objects, since the
#' serialization of these objects may not capture relevant changes.
#'
#'
#' @section Cache keys and reactivity:
#'
#'   Because the **value** expression (from the original [reactive()]) is
#'   cached, it is not necessarily re-executed when someone retrieves a value,
#'   and therefore it can't be used to decide what objects to take reactive
#'   dependencies on. Instead, the **key** is used to figure out which objects
#'   to take reactive dependencies on. In short, the key expression is reactive,
#'   and value expression is no longer reactive.
#'
#'   Here's an example of what not to do: if the key is `input$x` and the value
#'   expression is from `reactive({input$x + input$y})`, then the resulting
#'   cached reactive  will only take a reactive dependency on `input$x` -- it
#'   won't recompute `{input$x + input$y}` when just `input$y` changes.
#'   Moreover, the cache won't use `input$y` as part of the key, and so it could
#'   return incorrect values in the future when it retrieves values from the
#'   cache. (See the examples below for an example of this.)
#'
#'   A better cache key would be something like `input$x, input$y`. This does
#'   two things: it ensures that a reactive dependency is taken on both
#'   `input$x` and `input$y`, and it also makes sure that both values are
#'   represented in the cache key.
#'
#'   In general, `key` should use the same reactive inputs as `value`, but the
#'   computation should be simpler. If there are other (non-reactive) values
#'   that are consumed, such as external data sources, they should be used in
#'   the `key` as well. Note that if the `key` is large, it can make sense to do
#'   some sort of reduction on it so that the serialization and hashing of the
#'   cache key is not too expensive.
#'
#'   Remember that the key is _reactive_, so it is not re-executed every single
#'   time that someone accesses the cached reactive. It is only re-executed if
#'   it has been invalidated by one of the reactives it depends on.. For
#'   example, suppose we have this cached reactive:
#'
#'   ```
#'   r <- cachedReactive(
#'     reactive({ input$x + input$y }),
#'     input$x, input$y
#'   )
#'   ```
#'
#' In this case, the key expression is essentially `reactive(list(input$x,
#' input$y))` (there's a bit more to it, but that's a good enough
#' approximation). The first time `r()` is called, it executes the key, then
#' fails to find it in the cache, so it executes the value expression, `{
#' input$x + input$y }`. If `r()` is called again, then it does not need to
#' re-execute the key expression, because it has not been invalidated via a
#' change to `input$x` or `input$y`; it simply returns the previous value.
#' However, if `input$x` or `input$y` changes, then the reactive expression will
#' be invalidated, and the next time that someone calls `r()`, the key
#' expression will need to be re-executed.
#'
#' Note that if the cached reactive is passed to [withEvent()], then the key
#' expression will no longer be reactive; instead, the event expression will
#' be reactive.
#'
#'
#' @section Async with cached reactives:
#'
#'  With a cached reactive expression, the key and/or value expression can be
#'  _asynchronous_. In other words, they can be promises -- not regular R
#'  promises, but rather objects provided by the \pkg{promises}  package, which
#'  are similar to promises in JavaScript. (See [promises::promise()] for more
#'  information.) You can also use [future::future()] objects to run code in a
#'  separate process or even on a remote machine.
#'
#'  If the value returns a promise, then anything that consumes the cached
#'  reactive must expect it to return a promise.
#'
#'  Similarly, if the key is a promise (in other words, if it is asynchronous),
#'  then the entire cached reactive must be asynchronous, since the key must be
#'  computed asynchronously before it knows whether to compute the value or the
#'  value is retrieved from the cache. Anything that consumes the cached
#'  reactive must therefore expect it to return a promise.
#'
#'
#' @section Cache scope:
#'
#'   By default, a cached reactive is scoped to the running application. That
#'   means that it shares a cache with all user sessions connected to the
#'   application (within the R process). This is done with the `cache`
#'   parameter's default value, `"app"`.
#'
#'   With an app-level cache scope, one user can benefit from the work done for
#'   another user's session. In most cases, this is the best way to get
#'   performance improvements from caching. However, in some cases, this could
#'   leak information between sessions. For example, if a user sees that a cached
#'   reactive returns its value very quickly, they may be able to infer that
#'   someone else has already used it with the same values. Or if the cache key
#'   does not fully encompass the inputs used by the value, then data could leak
#'   between the sessions.
#'
#'   It is also possible to scope the cache to the session, with
#'   `cache="session"`. This removes the risk of information leaking between
#'   sessions, but then one session cannot benefit from computations performed in
#'   another session.
#'
#'   Finally, it is possible to pass in caching objects directly to
#'   `withCache()`. This can be useful if, for example, you want to use a
#'   particular type of cache with specific cached reactives.
#'
#'
#' @section Cache key internals:
#'
#'   The actual cache key that is used internally takes value from evaluating the
#'   key expression(s) and combines it with the (unevaluated) value expression.
#'
#'   This means that if there are two cached reactives which have the same result
#'   from evaluating the key, but different value expressions, then they will not
#'   need to worry about collisions.
#'
#'   However, if two cached reactives have identical key and value expressions
#'   expressions, they will share the cached values. This is useful when using
#'   `cache="app"`: there may be multiple user sessions which create separate
#'   cached reactive objects (because they are created from the same code in the
#'   server function, but the server function is executed once for each user
#'   session), and those cached reactive objects across sessions can share values
#'   in the cache.
#'
#' @param x The object to add caching to.
#' @param ... One or more expressions to use in the caching key.
#' @param cache The scope of the cache, or a cache object. This can be `"app"`
#'   (the default), `"session"`, or a cache object like a [cachem::cache_disk()].
#'   See the Cache Scoping section for more information.
#'
#' @seealso [withEvent()]
#'
#' @examples
#' \dontrun{
#' rc <- withCache(
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
#'   withCache(input$x)
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
#'       withCache(input$x, input$y)
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
#'       withCache(input$x, input$y)
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
#'       withCache(input$x, input$y) %>%
#'       withEvent(input$go)
#'       # The cached, eventified reactive takes a reactive dependency on
#'       # input$go, but doesn't use it for the cache key. It uses input$x and
#'       # input$y for the cache key, but doesn't take a reactive depdency on
#'       # them, because the reactive dependency is superseded by addEvent().
#'
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#' }
#'
#'@export
withCache <- function(x, ..., cache = "app") {
  check_dots_unnamed()
  force(cache)

  UseMethod("withCache")
}

#' @export
withCache.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}

#' @export
withCache.reactiveExpr <- function(x, ..., cache = "app") {
  label <- exprToLabel(substitute(key), "cachedReactive")
  domain <- reactive_get_domain(x)

  keyFunc <- make_quos_func(enquos(...))

  valueFunc <- reactive_get_value_func(x)
  # Hash the value expression now -- this will be added to the key later on, to
  # reduce the chance of key collisions with other cachedReactives. Remove
  # source refs because they can differ even though the code is the same.
  valueExprHash <- digest(formalsAndBody(valueFunc), algo = "spookyhash")
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveValueFunc", ..stacktraceon = TRUE)

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)
  # Hacky workaround for issue with `%>%` preventing GC:
  # https://github.com/tidyverse/magrittr/issues/229
  if (exists(".GenericCallEnv") && exists(".", envir = .GenericCallEnv)) {
    rm(list = ".", envir = .GenericCallEnv)
  }


  res <- reactive(label = label, domain = domain, {
    hybrid_chain(
      keyFunc(),
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
      }
    )
  })

  class(res) <- c("reactive.cache", class(res))
  res
}

#' @export
withCache.shiny.render.function <- function(x, ..., cache = "app") {
  if (isFALSE(attr(x, "cacheable"))) {
    stop("Cannot call `withCache()` on this render function because it is marked as not cacheable.")
  }

  keyFunc <- make_quos_func(enquos(...))

  valueExprHash <- digest(list(
    formalsAndBody(extractUserRenderFunc(x)),
    formalsAndBody(attr(x, "outputFunc", exact = TRUE))
  ), algo = "spookyhash")

  valueFunc <- x

  res <- function(...) {
    domain <- getDefaultReactiveDomain()

    hybrid_chain(
      keyFunc(),
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
      }
    )
  }

  class(res) <- c("shiny.render.function.cache", class(x))
  res
}

#' @export
withCache.reactive.cache <- function(x, ...) {
  stop("withCache() has already been called on the object.")
}

#' @export
withCache.shiny.render.function.cache <- withCache.reactive.cache

#' @export
withCache.reactive.event <- function(x, ...) {
  stop("Can't call withCache() after calling withEvent() on an object. Maybe you wanted to call withEvent() after withCache()?")
}

#' @export
withCache.shiny.render.function.event <- withCache.reactive.event

#' @export
withCache.Observer <- function(x, ...) {
  stop("Can't withCache an observer, because observers exist for the side efects, not for their return values.")
}

#' @export
withCache.function <- function(x, ...) {
  stop(
    "Don't know how to add caching to a plain function. ",
    "If this is a render* function for Shiny, it may need to be updated. ",
    "Please see ?shiny::withCache for more information."
  )
}



# Given a list of quosures, return a function that will evaluate them and return
# the list. If the list contains a single quosure, unwrap it from the list.
make_quos_func <- function(quos) {
  if (length(quos) == 0) {
    stop("Need at least one expression in `...` to use as cache key or event.")
  }
  if (length(quos) == 1) {
    # Special case for one key expr. This is needed for async to work -- that
    # is, when the expr returns a promise. It needs to not be wrapped into a
    # list for the hybrid_chain stuff to detect that it's a promise. (Plus,
    # it's not even clear what it would mean to mix promises and non-promises
    # in the key.)
    quos <- quos[[1]]
    function() { eval_tidy(quos) }

  } else {
    function() { lapply(quos, eval_tidy) }
  }
}


# Given the render function, this extracts the original function created from
# expression that the user passed to the render function. So if they called
# `renderText({ a + b })`, this would return `function() { a + b })`.
extractUserRenderFunc <- function(func) {
  wrapped <- attr(func, "origRenderFunc", exact = TRUE)
  if (is.null(wrapped)) {
    warning("origRenderFunc attribute not found. ",
            "Caching may not work properly for this render function.")
    return(func)
  }

  unwrapped <- attr(wrapped, "wrappedFunc", exact = TRUE)
  if (is.null(unwrapped)) {
    warning("wrappedFunc attribute not found. ",
            "Caching may not work properly for this render function.")
    return(wrapped)
  }

  unwrapped
}


# Get the formals and body for a function, without source refs. This is used for
# consistent hashing of the function.
formalsAndBody <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  list(
    formals(x),
    body(utils::removeSource(x))
  )
}
