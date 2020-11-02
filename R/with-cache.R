#' Cache decorator
#' @export
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
  valueExprHash <- digest(body(utils::removeSource(valueFunc)), algo = "spookyhash")
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveValueFunc", ..stacktraceon = TRUE)

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)
  # Hacky workaround for issue with `%>%` preventing GC:
  # https://github.com/tidyverse/magrittr/issues/229
  if (exists(".GenericCallEnv") && exists(".", envir = .GenericCallEnv)) {
    rm(., envir = .GenericCallEnv)
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
  keyFunc <- make_quos_func(enquos(...))

  valueExprHash <- "TODO"
  valueFunc <- x

  res <- function(...) {
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
withCache.reactive.event <- function(x, ...) {
  stop("Can't call withCache() after calling withEvent() on an object. Maybe you wanted to call withEvent() after withCache()?")
}

#' @export
withCache.shiny.render.function.event <- withCache.reactive.event

#' @export
withCache.Observer <- function(x, ...) {
  stop("Can't withCache an observer, because observers exist for the side efects, not for their return values.")
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
