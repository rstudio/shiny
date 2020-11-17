utils::globalVariables(".GenericCallEnv", add = TRUE)

#' Add caching with reactivity to an object
#'
#' @description
#'
#' `bindCache()` adds persistent caching to the following kinds of objects used
#' in Shiny:
#'
#' * [reactive()] expressions.
#' * `render*` functions, like [renderText()],
#' [renderTable()], and so on.
#'
#' Ordinary [reactive()] expressions automatically cache and reuse their _most
#' recent_ value, which helps to avoid redundant computation in downstream
#' reactive expressions). However, they have no ability to cache a _history_ of
#' values (i.e., they leverage a _transient_ cache, but have no inherent ability
#' to leverage a _persistent_ cache). `bindCache()` adds a persistent cache to
#' reactive objects, so that computations may be shared across (or within) user
#' sessions. As a result, `bindCache()` can dramatically improve performance,
#' but it does take additional effort to be implemented properly.
#'
#' `bindCache()` requires a _cache key_ -- this key is used to determine if a
#' computation has occurred before and hence can be retrieved from the cache. If
#' you're familiar with the concept of memoizing pure functions (e.g., the
#' \pkg{memoise} package), you can think of the cache key as the input(s) to a
#' pure function. As such, one should take care to make sure the use of
#' `bindCache()` is _pure_ in the same sense, namely:
#'
#' 1. The return value (e.g., `3`) is the same for the same key (e.g., `input$x = 1` & `input$y = 2`)
#' 2. Evaluation has no side-effects.
#'
#' ```
#' r <- reactive({ input$x + input$y }) %>%
#'   bindCache(input$x, input$y)
#' ```
#'
#' The largest performance improvements occur when the cache key is fast to
#' compute and the reactive expression is slow to compute. To compute the cache
#' key, `bindCache()` must [hash](https://en.wikipedia.org/wiki/Hash_function)
#' the contents of `...`, so it's best to avoid including large objects in a
#' cache key since that can will result in slow hashing. It's also best to avoid
#' reference objects (e.g., [environment()], [R6::R6()], etc) since computing
#' the hash requires serialization (unexpected things can happen when
#' serializing reference objects).
#'
#' For `reactive()`s that are really slow, it's often natural to pair [bindCache()]
#' with [bindEvent()] so that no computation is performed until the user
#' explicitly requests it (for more, see the Details section of [bindEvent()]).
#'
#' @section Cache scope:
#'
#'   By default, a `bindCache()` is scoped to the running application. That
#'   means that it shares a cache with all user sessions connected to the
#'   application (within the R process). This is done with the `cache`
#'   parameter's default value, `"app"`.
#'
#'   With an app-level cache scope, one user can benefit from the work done for
#'   another user's session. In most cases, this is the best way to get
#'   performance improvements from caching. However, in some cases, this could
#'   leak information between sessions. For example, if the cache key does not
#'   fully encompass the inputs used by the value (i.e., it's not pure), then data could leak between
#'   the sessions. Or if a user sees that a cached reactive returns its value
#'   very quickly, they may be able to infer that someone else has already used
#'   it with the same values.
#'
#'   It is also possible to scope the cache to the session, with
#'   `cache="session"`. This removes the risk of information leaking between
#'   sessions, but then one session cannot benefit from computations performed in
#'   another session.
#'
#'   It is possible to pass in caching objects directly to
#'   `bindCache()`. This can be useful if, for example, you want to use a
#'   particular type of cache with specific cached reactives, or if you want to
#'   use a [cachem::cache_disk()] that is shared across multiple processes and
#'   persists beyond the current R session.
#'
#'   To use different settings for an application-scoped cache, you can call
#'   [shinyOptions()] at the top of your app.R, server.R, or
#'   global.R. For example, this will create a cache with 500 MB of space
#'   instead of the default 200 MB:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_mem(size = 500e6))
#'   ```
#'
#'   To use different settings for a session-scoped cache, you can set
#'   `self$cache` at the top of your server function. By default, it will create
#'   a 200 MB memory cache for each session, but you can replace it with
#'   something different. To use the session-scoped cache, you must also call
#'   `bindCache()` with `cache="session"`. This will create a 100 MB cache for
#'   the session:
#'
#'   ```
#'   function(input, output, session) {
#'     session$cache <- cachem::cache_mem(size = 100e6)
#'     ...
#'   }
#'   ```
#'
#'   If you want to use a cache that is shared across multiple R processes, you
#'   can use a [cachem::cache_disk()]. You can create a application-level shared
#'   cache by putting this at the top of your app.R, server.R, or global.R:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache"))
#'   ```
#'
#'   This will create a subdirectory in your system temp directory named
#'   `myapp-cache` (replace `myapp-cache` with a unique name of
#'   your choosing). On most platforms, this directory will be removed when
#'   your system reboots. This cache will persist across multiple starts and
#'   stops of the R process, as long as you do not reboot.
#'
#'   To have the cache persist even across multiple reboots, you can create the
#'   cache in a location outside of the temp directory. For example, it could
#'   be a subdirectory of the application:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_disk("./myapp-cache"))
#'   ```
#'
#'   In this case, resetting the cache will have to be done manually, by deleting
#'   the directory.
#'
#'   You can also scope a cache to just one item, or selected items. To do that,
#'   create a [cachem::cache_mem()] or [cachem::cache_disk()], and pass it
#'   as the `cache` argument of `bindCache()`.
#'
#' @section Async with cached reactives:
#'
#'   With a cached reactive expression, the key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises --- not regular R
#'   promises, but rather objects provided by the
#'   \href{https://rstudio.github.io/promises/}{\pkg{promises}}  package, which
#'   are similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.) You can also use [future::future()] objects to run code in a
#'   separate process or even on a remote machine.
#'
#'   If the value returns a promise, then anything that consumes the cached
#'   reactive must expect it to return a promise.
#'
#'   Similarly, if the key is a promise (in other words, if it is asynchronous),
#'   then the entire cached reactive must be asynchronous, since the key must be
#'   computed asynchronously before it knows whether to compute the value or the
#'   value is retrieved from the cache. Anything that consumes the cached
#'   reactive must therefore expect it to return a promise.
#'
#' @section Computing cache keys:
#'
#'   When [bindEvent()] computes a cache key, it combines key expression(s)
#'   (`...`) with the (unevaluated) reactive expression its bound to.
#'   As a result, two different reactives with the same key won't read from the same
#'   cache, unless they have exactly the same code expression (the latter case
#'   is important for sharing cache when `cache="app"` where separate
#'   cached reactive objects are created for each user).
#'
#' @section Developing render functions for caching:
#'
#'   If you've implemented your own `render*()` function, you may need to
#'   provide information to [createRenderFunction()] (or
#'   `htmlwidgets::shinyRenderWidget()`, if you've authored an
#'   \pkg{htmlwidgets}) in order for `bindCache()` to correctly compute a cache
#'   key. In general, it's best practice to provide a `label` id, the user's
#'   `expr`, as well as any other arguments that may influence the final value
#'   (any the idea here is to produce a pure cache key).
#'
#'   ```
#'   myWidget <- function(expr, ...) {
#'     hint <- list(label = "myWidget", userExpr = expr, ...)
#'     htmlwidgets::shinyRenderWidget(expr, myWidgetOutput, cacheHint = hint)
#'   }
#'   ```
#'
#'   In some cases, a pure cache key can be automatically derived, so explicitly
#'   providing the `cacheHint` may not be needed. To check if it is needed, call
#'   the internal `shiny:::extractCacheHint()` on the `render*()` function to
#'   see what information is included in the cache key. At the very least, this
#'   should include the user's expression `{ input$x }`.
#'
#'   ```
#'   shiny:::extractCacheHint(myWidget({ input$x }))
#'   ```
#'
#' @section Uncacheable objects:
#'
#'   Some render functions cannot be cached, typically because they have side
#'   effects or modify some external state, and they must re-execute each time
#'   in order to work properly.
#'
#'   For developers of such code, they should call [createRenderFunction()] or
#'   [markRenderFunction()] with `cacheHint = FALSE`.
#'
#'
#' @param x The object to add caching to.
#' @param ... One or more expressions to use in the caching key.
#' @param cache The scope of the cache, or a cache object. This can be `"app"`
#'   (the default), `"session"`, or a cache object like a [cachem::cache_disk()].
#'   See the Cache Scoping section for more information.
#'
#' @seealso [bindEvent()], [renderCachedPlot()] for caching plots.
#'
#' @examples
#' \dontrun{
#' rc <- bindCache(
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
#'   bindCache(input$x)
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
#'       bindCache(input$x, input$y)
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
#'       bindCache(input$x, input$y)
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
#'       bindCache(input$x, input$y) %>%
#'       bindEvent(input$go)
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
bindCache <- function(x, ..., cache = "app") {
  check_dots_unnamed()
  force(cache)

  UseMethod("bindCache")
}

#' @export
bindCache.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}

#' @export
bindCache.reactiveExpr <- function(x, ..., cache = "app") {
  label <- exprToLabel(substitute(key), "cachedReactive")
  domain <- reactive_get_domain(x)

  # Convert the ... to a function that returns their evaluated values.
  keyFunc <- exprs_to_func(dot_exprs(), parent.frame())

  valueFunc <- reactive_get_value_func(x)
  # Hash cache hint now -- this will be added to the key later on, to reduce the
  # chance of key collisions with other cachedReactives.
  cacheHint <- digest(extractCacheHint(x), algo = "spookyhash")
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
        key_str <- digest(list(cacheKeyResult, cacheHint), algo = "spookyhash")
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
bindCache.shiny.render.function <- function(x, ..., cache = "app") {
  keyFunc <- exprs_to_func(dot_exprs(), parent.frame())

  cacheHint <- digest(extractCacheHint(x), algo = "spookyhash")

  valueFunc <- x

  res <- function(...) {
    domain <- getDefaultReactiveDomain()

    hybrid_chain(
      keyFunc(),
      function(cacheKeyResult) {
        cache <- resolve_cache_object(cache, domain)
        key_str <- digest(list(cacheKeyResult, cacheHint), algo = "spookyhash")
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
          withVisible(isolate(valueFunc(...))),
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
bindCache.reactive.cache <- function(x, ...) {
  stop("bindCache() has already been called on the object.")
}

#' @export
bindCache.shiny.render.function.cache <- bindCache.reactive.cache

#' @export
bindCache.reactive.event <- function(x, ...) {
  stop("Can't call bindCache() after calling bindEvent() on an object. Maybe you wanted to call bindEvent() after bindCache()?")
}

#' @export
bindCache.shiny.render.function.event <- bindCache.reactive.event

#' @export
bindCache.Observer <- function(x, ...) {
  stop("Can't bindCache an observer, because observers exist for the side efects, not for their return values.")
}

#' @export
bindCache.function <- function(x, ...) {
  stop(
    "Don't know how to add caching to a plain function. ",
    "If this is a render* function for Shiny, it may need to be updated. ",
    "Please see ?shiny::bindCache for more information."
  )
}


extractCacheHint <- function(func) {
  cacheHint <- attr(func, "cacheHint", exact = TRUE)

  if (is_false(cacheHint)) {
    stop(
      "Cannot call `bindCache()` on this object because it is marked as not cacheable.",
      call. = FALSE
    )
  }

  if (is.null(cacheHint)) {
    warning("No cacheHint found for this object. ",
            "Caching may not work properly.")
  }

  cacheHint
}
