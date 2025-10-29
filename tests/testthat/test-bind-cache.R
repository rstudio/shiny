
test_that("bindCache reactive basic functionality", {
  cache <- cachem::cache_mem()

  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    vals <<- c(vals, x)
    k()
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
  }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })

  flushReact()
  expect_identical(vals, c("0k", "0v", "0o"))

  vals <- character()
  k(1)
  flushReact()
  k(2)
  flushReact()
  expect_identical(vals, c("1k", "1v", "1o", "2k", "2v", "2o"))

  # Use a value that is in the cache. k and o will re-execute, but v will not.
  vals <- character(0)
  k(1)
  flushReact()
  expect_identical(vals, c("1k", "1o"))
  k(0)
  flushReact()
  expect_identical(vals, c("1k", "1o", "0k", "0o"))

  # Reset the cache - k and v will re-execute even if it's a previously-used value.
  vals <- character(0)
  cache$reset()
  k(1)
  flushReact()
  expect_identical(vals, c("1k","1v", "1o"))
})

test_that("bindCache - multiple key expressions", {
  cache <- cachem::cache_mem()

  k1 <- reactiveVal(0)
  k2 <- reactiveVal(0)

  r_vals <- character()
  r <- reactive({
    x <- paste0(k1(), ":", k2())
    r_vals <<- c(r_vals, x)
    x
  }) %>%
    bindCache(k1(), k2(), cache = cache)

  o_vals <- character()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  flushReact()
  expect_identical(r_vals, "0:0")
  expect_identical(o_vals, "0:0")
  flushReact()
  expect_identical(r_vals, "0:0")
  expect_identical(o_vals, "0:0")

  # Each of the items can trigger
  r_vals <- character(); o_vals <- character()
  k1(10)
  flushReact()
  expect_identical(r_vals, "10:0")
  expect_identical(o_vals, "10:0")

  r_vals <- character(); o_vals <- character()
  k2(100)
  flushReact()
  expect_identical(r_vals, "10:100")
  expect_identical(o_vals, "10:100")

  # Using a cached value means that reactive won't execute
  r_vals <- character(); o_vals <- character()
  k2(0)
  flushReact()
  expect_identical(r_vals, character())
  expect_identical(o_vals, "10:0")
  k1(0)
  flushReact()
  expect_identical(r_vals, character())
  expect_identical(o_vals, c("10:0", "0:0"))
})


test_that("bindCache reactive - original reactive can be GC'd", {
  # bindCache.reactive essentially extracts code from the original reactive and
  # then doesn't need the original anymore. We want to make sure the original
  # can be GC'd afterward (if no one else has a reference to it).
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({ k() })

  finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) finalized <<- TRUE)

  r1 <- r %>% bindCache(k(), cache = cache)
  rm(r)
  gc()
  expect_true(finalized)


  # Same, but when using rlang::inject() to insert a quosure
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  exp <- quo({ k() })
  r <- inject(reactive(!!exp))

  finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) finalized <<- TRUE)

  r1 <- r %>% bindCache(k(), cache = cache)
  rm(r)
  gc()
  expect_true(finalized)
})

test_that("bindCache reactive - value is isolated", {
  # The value is isolated; the key is the one that dependencies are taken on.
  cache <- cachem::cache_mem()

  k <- reactiveVal(1)
  v <- reactiveVal(10)

  vals <- character()
  r <- reactive({
    x <- paste0(v(), "v")
    vals <<- c(vals, x)
    v()
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
  }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })

  flushReact()
  expect_identical(vals, c("1k", "10v", "10o"))

  # Changing k() triggers reactivity
  k(2)
  flushReact()
  k(3)
  flushReact()
  expect_identical(vals, c("1k", "10v", "10o", "2k", "10v", "10o", "3k", "10v", "10o"))

  # Changing v() does not trigger reactivity
  vals <- character()
  v(20)
  flushReact()
  v(30)
  flushReact()
  expect_identical(vals, character())

  # If k() changes, it will invalidate r, which will invalidate o. r will not
  # re-execute, but instead fetch the old value (10) from the cache (from when
  # the key was 1), and that value will be passed to o. This is an example of a
  # bad key!
  k(1)
  flushReact()
  expect_identical(vals, c("1k", "10o"))

  # A new un-cached value for v will cause r to re-execute; it will fetch the
  # current value of v (30), and that value will be passed to o.
  vals <- character()
  k(4)
  flushReact()
  expect_identical(vals, c("4k", "30v", "30o"))
})


# ============================================================================
# Async key
# ============================================================================
test_that("bindCache reactive with async key", {
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    vals <<- c(vals, x)
    k()
  }) %>% bindCache({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "k1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(k(), "k2")
      vals <<- c(vals, x)
      value
    })
  }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })
  })

  # Initially, only the first step in the promise for key runs.
  flushReact()
  expect_identical(vals, c("0k1"))

  # After pumping the event loop a feww times, the rest of the chain will run.
  for (i in 1:3) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0v", "0o"))

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  vals <- character()
  k(1)
  flushReact()
  expect_identical(vals, c("1k1"))
  for (i in 1:3) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "1v", "1o"))

  # Going back to a cached value: The reactive's expr won't run, but the
  # observer will.
  vals <- character()
  k(0)
  flushReact()
  expect_identical(vals, c("0k1"))
  for (i in 1:3) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0o"))
})


# ============================================================================
# Async value
# ============================================================================
test_that("bindCache reactives with async value", {
  # If the value expr returns a promise, it must return a promise every time,
  # even when the value is fetched in the cache. Similarly, if it returns a
  # non-promise value, then it needs to do that whether or not it's fetched from
  # the cache. This tests the promise case (almost all the other tests here test
  # the non-promise case).

  # Async value
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()

  r <- reactive({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "v1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(value, "v2")
      vals <<- c(vals, x)
      value
    })
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
  }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })
  })

  # Initially, the `then` in the value expr and observer don't run, but they will
  # after running the event loop.
  flushReact()
  expect_identical(vals, c("0k", "0v1"))
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k", "0v1", "0v2", "0o"))

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  vals <- character()
  k(1)
  flushReact()
  expect_identical(vals, c("1k", "1v1"))
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("1k", "1v1", "1v2", "1o"))

  # Going back to a cached value: The reactives's expr won't run, but the
  # observer will.
  vals <- character()
  k(0)
  flushReact()
  expect_identical(vals, c("0k"))
  for (i in 1:2) later::run_now()
  expect_identical(vals, c("0k", "0o"))
})


# ============================================================================
# Async key and value
# ============================================================================
test_that("bindCache reactives with async key and value", {
  # If the value expr returns a promise, it must return a promise every time,
  # even when the value is fetched in the cache. Similarly, if it returns a
  # non-promise value, then it needs to do that whether or not it's fetched from
  # the cache. This tests the promise case (almost all the other tests here test
  # the non-promise case).

  # Async key and value
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()

  r <- reactive({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "v1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(value, "v2")
      vals <<- c(vals, x)
      value
    })
  }) %>% bindCache({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "k1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(k(), "k2")
      vals <<- c(vals, x)
      value
    })
  }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })
  })

  flushReact()
  expect_identical(vals, c("0k1"))
  for (i in 1:8) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0v1", "0v2", "0o"))

  # If we change k, we should see same pattern as above.
  vals <- character(0)
  k(1)
  flushReact()
  expect_identical(vals, c("1k1"))
  for (i in 1:8) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "1v1", "1v2", "1o"))

  # Going back to a cached value: The reactive's expr won't run, but the
  # observer will.
  vals <- character(0)
  k(0)
  flushReact()
  expect_identical(vals, c("0k1"))
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0o"))
})

test_that("bindCache reactive key collisions", {
  # =======================================
  # No collision with different value exprs
  # =======================================
  cache <- cachem::cache_mem()
  k <- reactiveVal(1)

  # Key collisions don't happen if they have different reactive expressions
  # (because that is used in the key).
  r_vals <- numeric()
  r1 <- reactive({
    val <- k() * 10
    r_vals <<- c(r_vals, val)
    val
  }) %>%
    bindCache(k(), cache = cache)

  r_vals <- numeric()
  r2 <- reactive({
    val <- k() * 100
    r_vals <<- c(r_vals, val)
    val
  }) %>%
    bindCache(k(), cache = cache)

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r1(), r2())
  })

  # No collision because the reactive's expr is used in the key
  flushReact()
  expect_identical(r_vals, c(10, 100))
  expect_identical(o_vals, c(10, 100))

  k(2)
  flushReact()
  expect_identical(r_vals, c(10, 100, 20, 200))
  expect_identical(o_vals, c(10, 100, 20, 200))


  # ====================================
  # Collision with identical value exprs
  # ====================================
  cache <- cachem::cache_mem()
  k <- reactiveVal(1)

  # Key collisions DO happen if they have the same value expressions.
  r_vals <- numeric()
  r1 <- reactive({
    val <- k() * 10
    r_vals <<- c(r_vals, val)
    val
  }) %>%
    bindCache(k(), cache = cache)

  r2 <- reactive({
    val <- k() * 10
    r_vals <<- c(r_vals, val)
    val
  }) %>%
    bindCache(k(), cache = cache)

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r1(), r2())
  })

  # r2() never actually runs -- key collision. This is good, because this is
  # what allows cache to be shared across multiple sessions.
  flushReact()
  expect_identical(r_vals, 10)
  expect_identical(o_vals, c(10, 10))

  k(2)
  flushReact()
  expect_identical(r_vals, c(10, 20))
  expect_identical(o_vals, c(10, 10, 20, 20))
})


# ============================================================================
# Error handling
# ============================================================================
test_that("bindCache reactive error handling", {
  # ===================================
  # Error in key
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  # Error in key
  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    k()
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
    stop("foo")
  }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })

  suppress_stacktrace(expect_warning(flushReact()))
  # A second flushReact should not raise warnings, since key has not been
  # invalidated.
  expect_silent(flushReact())

  k(1)
  suppress_stacktrace(expect_warning(flushReact()))
  expect_silent(flushReact())
  k(0)
  suppress_stacktrace(expect_warning(flushReact()))
  expect_silent(flushReact())
  # value expr and observer shouldn't have changed at all
  expect_identical(vals, c("0k", "1k", "0k"))

  # ===================================
  # Silent error in key with req(FALSE)
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    k()
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
    req(FALSE)
  }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })


  expect_silent(flushReact())
  k(1)
  expect_silent(flushReact())
  k(0)
  expect_silent(flushReact())
  # value expr and observer shouldn't have changed at all
  expect_identical(vals, c("0k", "1k", "0k"))

  # ===================================
  # Error in value
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    vals <<- c(vals, x)
    stop("foo")
    k()
  }) %>%
    bindCache({
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })

  suppress_stacktrace(expect_warning(flushReact()))
  expect_silent(flushReact())
  k(1)
  suppress_stacktrace(expect_warning(flushReact()))
  expect_silent(flushReact())
  k(0)
  # Should re-throw cached error
  suppress_stacktrace(expect_warning(flushReact()))
  expect_silent(flushReact())

  # 0v shouldn't be present, because error should be re-thrown without
  # re-running code.
  expect_identical(vals, c("0k", "0v", "1k", "1v", "0k"))

  # =====================================
  # Silent error in value with req(FALSE)
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)

  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    vals <<- c(vals, x)
    req(FALSE)
    k()
  }) %>% bindCache({
    x <- paste0(k(), "k")
    vals <<- c(vals, x)
    k()
  }, cache = cache)

  o <- observe({
    x <- paste0(r(), "o")
    vals <<- c(vals, x)
  })

  expect_silent(flushReact())
  k(1)
  expect_silent(flushReact())
  k(0)
  # Should re-throw cached error
  expect_silent(flushReact())

  # 0v shouldn't be present, because error should be re-thrown without
  # re-running code.
  expect_identical(vals, c("0k", "0v", "1k", "1v", "0k"))
})


test_that("bindCache reactive error handling - async", {
  # ===================================
  # Error in key
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  vals <- character()
  r <- reactive({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "v1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(value, "v2")
      vals <<- c(vals, x)
      value
    })
  }) %>% bindCache({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "k1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(k(), "k2")
      vals <<- c(vals, x)
      stop("err", k())
      value
    })
  },
    cache = cache
  )

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })$catch(function(value) {
      x <- paste0(value$message, "oc")
      vals <<- c(vals, x)
    })
  })

  suppress_stacktrace(flushReact())
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "err0oc"))

  # A second flushReact should not raise warnings, since key has not been
  # invalidated.
  expect_silent(flushReact())

  vals <- character()
  k(1)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "err1oc"))

  vals <- character()
  k(0)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "err0oc"))

  # ===================================
  # Silent error in key with req(FALSE)
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  vals <- character()
  r <- reactive({
    x <- paste0(k(), "v")
    vals <<- c(vals, x)
    resolve(k())
  }) %>% bindCache({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "k1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(k(), "k2")
      vals <<- c(vals, x)
      req(FALSE)
      value
    })
  }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })$catch(function(value) {
      x <- paste0(value$message, "oc")
      vals <<- c(vals, x)
    })
  })

  suppress_stacktrace(flushReact())
  for (i in 1:4) later::run_now()
  # The `catch` will receive an empty message
  expect_identical(vals, c("0k1", "0k2", "oc"))

  # A second flushReact should not raise warnings, since key has not
  # been invalidated.
  expect_silent(flushReact())

  vals <- character()
  k(1)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "oc"))

  vals <- character()
  k(0)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "oc"))

  # ===================================
  # Error in value
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  vals <- character()
  r <- reactive({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "v1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(value, "v2")
      vals <<- c(vals, x)
      stop("err", k())
      value
    })
  }) %>% bindCache({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "k1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(k(), "k2")
      vals <<- c(vals, x)
      value
    })
  }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })$catch(function(value) {
      x <- paste0(value$message, "oc")
      vals <<- c(vals, x)
    })
  })

  suppress_stacktrace(flushReact())
  for (i in 1:9) later::run_now()
  # A second flushReact should not raise warnings, since key has not been
  # invalidated.
  expect_silent(flushReact())
  expect_identical(vals, c("0k1", "0k2", "0v1", "0v2", "err0oc"))

  vals <- character()
  k(1)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:9) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "1v1", "1v2", "err1oc"))

  vals <- character()
  k(0)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "err0oc"))

  # =====================================
  # Silent error in value with req(FALSE)
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  vals <- character()
  r <- reactive({
    promises::promise(function(resolve, reject) {
      x <- paste0(k(), "v1")
      vals <<- c(vals, x)
      resolve(k())
    })$then(function(value) {
      x <- paste0(value, "v2")
      vals <<- c(vals, x)
      req(FALSE)
      value
    })
  }) %>%
    bindCache({
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "k1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(k(), "k2")
        vals <<- c(vals, x)
        value
      })
    }, cache = cache)

  o <- observe({
    r()$then(function(value) {
      x <- paste0(value, "o")
      vals <<- c(vals, x)
    })$catch(function(value) {
      x <- paste0(value$message, "oc")
      vals <<- c(vals, x)
    })
  })

  suppress_stacktrace(flushReact())
  for (i in 1:9) later::run_now()
  # A second flushReact should not raise warnings, since key has not been
  # invalidated.
  expect_silent(flushReact())
  expect_identical(vals, c("0k1", "0k2", "0v1", "0v2", "oc"))

  vals <- character()
  k(1)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:9) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "1v1", "1v2", "oc"))

  vals <- character()
  k(0)
  suppress_stacktrace(flushReact())
  expect_silent(flushReact())
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "oc"))
})


# ============================================================================
# Quosures
# ============================================================================
test_that("bindCache quosures -- inlined with inject() at creation time", {
  cache <- cachem::cache_mem()
  res <- NULL
  a <- 1
  r <- inject({
    reactive({
        eval_tidy(quo(!!a))
      }) %>%
      bindCache({
        x <- eval_tidy(quo(!!a)) + 10
        res <<- x
        x
      }, cache = cache)
  })
  a <- 2
  expect_identical(isolate(r()), 1)
  expect_identical(res, 11)
})


test_that("bindCache quosures -- unwrapped at execution time", {
  cache <- cachem::cache_mem()
  res <- NULL
  a <- 1
  r <- reactive({
      eval_tidy(quo(!!a))
    }) %>%
    bindCache({
      x <- eval_tidy(quo(!!a)) + 10
      res <<- x
      x
    }, cache = cache)
  a <- 2
  expect_identical(isolate(r()), 2)
  expect_identical(res, 12)
})


# ============================================================================
# Visibility
# ============================================================================
test_that("bindCache visibility", {
  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  res <- NULL
  r <- bindCache(k(), cache = cache,
    x = reactive({
      if (k() == 0) invisible(k())
      else          k()
    })
  )

  o <- observe({
    res <<- withVisible(r())
  })

  flushReact()
  expect_identical(res, list(value = 0, visible = FALSE))
  k(1)
  flushReact()
  expect_identical(res, list(value = 1, visible = TRUE))
  # Now fetch from cache
  k(0)
  flushReact()
  expect_identical(res, list(value = 0, visible = FALSE))
  k(1)
  flushReact()
  expect_identical(res, list(value = 1, visible = TRUE))
})


test_that("bindCache reactive visibility - async", {
  # only test if promises handles visibility
  skip_if_not_installed("promises", "1.1.1.9001")

  cache <- cachem::cache_mem()
  k <- reactiveVal(0)
  res <- NULL
  r <- reactive({
    promise(function(resolve, reject) {
      if (k() == 0) resolve(invisible(k()))
      else          resolve(k())
    })
  }) %>%
    bindCache(k(), cache = cache)

  o <- observe({
    r()$then(function(value) {
      res <<- withVisible(value)
    })
  })

  flush_and_run_later <- function(k) {
    flushReact()
    for (i in 1:k) later::run_now()
  }

  flush_and_run_later(4)
  expect_identical(res, list(value = 0, visible = FALSE))

  k(1)
  flush_and_run_later(4)
  expect_identical(res, list(value = 1, visible = TRUE))

  # Now fetch from cache
  k(0)
  flush_and_run_later(4)
  expect_identical(res, list(value = 0, visible = FALSE))

  k(1)
  flush_and_run_later(4)
  expect_identical(res, list(value = 1, visible = TRUE))
})


# ============================================================================
# bindCache and render functions
# ============================================================================

test_that("bindCache renderFunction basic functionality", {
  m <- cachem::cache_mem()
  n <- 0 # Counter for how many times renderFunctions run.
  a <- 1

  # Two renderTexts with the same expression should share cache
  t1 <- renderText({ n <<- n+1; a + 1 }) %>% bindCache(a, cache = m)
  t2 <- renderText({ n <<- n+1; a + 1 }) %>% bindCache(a, cache = m)
  expect_identical(t1(), "2")
  expect_identical(t2(), "2")
  expect_identical(n, 1)

  a <- 2
  expect_identical(t1(), "3")
  expect_identical(t2(), "3")
  expect_identical(n, 2)

  # renderPrint with the same expression -- should run, and have a different
  # result.
  p1 <- renderPrint({ n <<- n+1; a + 1 }) %>% bindCache(a, cache = m)
  p2 <- renderPrint({ n <<- n+1; a + 1 }) %>% bindCache(a, cache = m)
  expect_identical(p1(), "[1] 3")
  expect_identical(p2(), "[1] 3")
  expect_identical(n, 3)
})

# ==============================================================================
# Custom render functions
# ==============================================================================
test_that("Custom render functions that call installExprFunction", {
  # Combinations with `installExprFunction` or `quoToFunction` plus
  # `markRenderFunction` or `createRenderFunction` should work.

  # The expressions passed into renderDouble below should be converted into this
  # function. We'll use this for comparison.
  target_cachehint <- list(
    origUserFunc = formalsAndBody(function() { n <<- n + 1; a }),
    renderFunc = list()
  )

  # installExprFunction + createRenderFunction: OK
  renderDouble <- function(expr) {
    installExprFunction(expr, "func")
    createRenderFunction(
      func,
      transform = function(value, session, name, ...) paste0(value, ",", value)
    )
  }
  n <- 0
  a <- 1
  tc <- renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem())
  expect_identical(tc(), "1,1")
  expect_identical(tc(), "1,1")
  expect_identical(n, 1)
  expect_identical(
    extractCacheHint(renderDouble({ n <<- n+1; a }))$origUserFunc,
    formalsAndBody(function() { n <<- n + 1; a })
  )


  # quoToFunction + createRenderFunction: OK
  renderDouble <- function(expr) {
    func <- quoToFunction(enquo(expr), "renderDouble")
    createRenderFunction(
      func,
      transform = function(value, session, name, ...) paste0(value, ",", value)
    )
  }
  # Should work, because it went through createRenderFunction().
  n <- 0
  a <- 1
  tc <- renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem())
  expect_identical(tc(), "1,1")
  expect_identical(tc(), "1,1")
  expect_identical(n, 1)
  expect_identical(
    extractCacheHint(renderDouble({ n <<- n+1; a }))$origUserFunc,
    formalsAndBody(function() { n <<- n + 1; a })
  )


  # installExprFunction + markRenderFunction (without cacheHint): warning
  # because the original function can't be automatically extracted (it was
  # wrapped by installExprFunction).
  renderDouble <- function(expr) {
    installExprFunction(expr, "func")
    markRenderFunction(textOutput, function() {
      value <- func()
      paste0(value, ",", value)
    })
  }
  expect_warning(renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem()))

  # installExprFunction + markRenderFunction (without cacheHint): warning
  # because the original function can't be automatically extracted (it was
  # wrapped by installExprFunction).
  renderDouble <- function(expr) {
    installExprFunction(expr, "func")
    markRenderFunction(textOutput,
      function() {
        value <- func()
        paste0(value, ",", value)
      },
      cacheHint = list(label = "renderDouble", userExpr = substitute(expr))
    )
  }
  n <- 0
  a <- 1
  tc <- renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem())
  extractCacheHint(renderDouble({ n <<- n+1; a }))
  expect_identical(tc(), "1,1")
  expect_identical(tc(), "1,1")
  expect_identical(n, 1)
  expect_identical(
    extractCacheHint(renderDouble({ n <<- n+1; a })),
    list(label = "renderDouble", userExpr = zap_srcref(quote({ n <<- n+1; a })))
  )


  # quoToFunction + markRenderFunction (without cacheHint): warning
  renderDouble <- function(expr) {
    func <- quoToFunction(enquo(expr), "renderDouble")
    markRenderFunction(textOutput, function() {
      value <- func()
      paste0(value, ",", value)
    })
  }
  expect_warning(renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem()))


  # quoToFunction + markRenderFunction (with cacheHint): OK
  # Also, non-list cacheHint will get wrapped into a list
  renderDouble <- function(expr) {
    func <- quoToFunction(enquo(expr), "renderDouble")
    markRenderFunction(textOutput,
      function() {
        value <- func()
        paste0(value, ",", value)
      },
      cacheHint = enexpr(expr)
    )
  }
  n <- 0
  a <- 1
  tc <- renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem())
  expect_identical(tc(), "1,1")
  expect_identical(tc(), "1,1")
  expect_identical(n, 1)
  expect_identical(
    extractCacheHint(renderDouble({ n <<- n+1; a })),
    list(zap_srcref(quote({ n <<- n + 1; a })))
  )


  # installExprFunction + nothing: error
  renderTriple <- function(expr) {
    installExprFunction(expr, "func")
    func
  }
  expect_error(renderTriple({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem()))

  # quoToFunction + nothing: error
  renderTriple <- function(expr) {
    quoToFunction(enquo(expr), "renderTriple")
  }
  expect_error(renderTriple({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem()))
})


test_that("cacheWriteHook and cacheReadHook for render functions", {
  testthat::skip_if(shiny_otel_tracer()$is_enabled(), "Skipping stack trace tests when OpenTelemetry is already enabled")

  write_hook_n <- 0
  read_hook_n  <- 0

  renderDouble <- function(expr) {
    func <- quoToFunction(enquo(expr), "renderDouble")
    createRenderFunction(
      func,
      transform = function(value, session, name, ...) paste0(value, ",", value),
      cacheWriteHook = function(value) {
        write_hook_n <<- write_hook_n + 1
        paste0(value, ",w")
      },
      cacheReadHook = function(value) {
        read_hook_n <<- read_hook_n + 1
        paste0(value, ",r")
      }
    )
  }

  n <- 0
  a <- 1
  tc <- renderDouble({ n <<- n+1; a }) %>% bindCache(a, cache = cachem::cache_mem())
  expect_identical(tc(), "1,1")
  expect_identical(write_hook_n, 1)
  expect_identical(read_hook_n, 0)
  expect_identical(tc(), "1,1,w,r")
  expect_identical(write_hook_n, 1)
  expect_identical(read_hook_n, 1)
  expect_identical(tc(), "1,1,w,r")
  expect_identical(write_hook_n, 1)
  expect_identical(read_hook_n, 2)
  expect_identical(n, 1)
})

test_that("Custom render functions that call exprToFunction", {
  # A render function that uses exprToFunction won't work with bindCache(). It
  # needs to use quoToFunction or installExprFunction.

  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- exprToFunction(expr, env, quoted)
    function() { value <- func(); paste0(value, ",", value) }
  }

  m <- cachem::cache_mem()
  # Should throw an error because bindCache doesn't know how to deal with plain
  # functions.
  expect_error(renderDouble({ a }) %>% bindCache(a, cache = m))

  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- exprToFunction(expr, env, quoted)
  }
  expect_error(renderDouble({ a }) %>% bindCache(a, cache = m))

  # exprToFunction + markRenderFunction: warning because exprToFunction
  # doesn't attach the original function as metadata.
  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- exprToFunction(expr, env, quoted)
    markRenderFunction(textOutput, func)
  }
  expect_warning(renderDouble({ a }) %>% bindCache(a, cache = m))

  # exprToFunction + createRenderFunction: warning because exprToFunction
  # doesn't attach the original function as metadata.
  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- exprToFunction(expr, env, quoted)
    createRenderFunction(func, outputFunc = textOutput)
  }
  expect_warning(renderDouble({ a }) %>% bindCache(a, cache = m))
})


test_that("Some render functions can't be cached", {
  withr::local_options(list(shiny.legacy.datatable = TRUE))

  m <- cachem::cache_mem()
  expect_error(renderDataTable({ cars }) %>% bindCache(1, cache = m))
  expect_error(renderCachedPlot({ plot(1) }, 1) %>% bindCache(1, cache = m))
  expect_error(renderImage({ cars }) %>% bindCache(1, cache = m))
})


test_that("cacheHint to avoid collisions", {
  # Same function and expression -> same cache hint
  expect_identical(
    extractCacheHint(renderText({ a + 1 })),
    extractCacheHint(renderText({ a + 1 })),
  )
  expect_identical(
    extractCacheHint(renderPrint({ a + 1 })),
    extractCacheHint(renderPrint({ a + 1 }))
  )
  expect_identical(
    extractCacheHint(renderUI({ a + 1 })),
    extractCacheHint(renderUI({ a + 1 }))
  )
  expect_identical(
    extractCacheHint(renderTable({ a + 1 })),
    extractCacheHint(renderTable({ a + 1 }))
  )

  # Different expressions -> different cache hint
  expect_false(identical(
    extractCacheHint(renderText({ a + 1 })),
    extractCacheHint(renderText({ a + 2 }))
  ))
  expect_false(identical(
    extractCacheHint(renderPrint({ a + 1 })),
    extractCacheHint(renderPrint({ a + 2 }))
  ))
  expect_false(identical(
    extractCacheHint(renderUI({ a + 1 })),
    extractCacheHint(renderUI({ a + 2 }))
  ))
  expect_false(identical(
    extractCacheHint(renderTable({ a + 1 })),
    extractCacheHint(renderTable({ a + 2 }))
  ))

  # Different functions -> different cache hint
  expect_false(identical(
    extractCacheHint(renderText({ a + 1 })),
    extractCacheHint(renderPrint({ a + 1 }))
  ))
  expect_false(identical(
    extractCacheHint(renderText({ a + 1 })),
    extractCacheHint(renderUI({ a + 1 }))
  ))
})


test_that("cacheHint works with quosures", {
  # Cache hint ignores environment
  my_quo <- local({
    a <- 5
    rlang::quo({a + 1})
  })
  ap1 <- rlang::expr({a+1})
  plotCacheList <- list(userExpr = ap1, res = 72)
  reactiveCacheList <- list(userExpr = ap1)
  quoCacheList <- list(q = ap1)


  # render**
  # Regular expression, quoted quosure object, injected quosure object
  expect_equal(
    extractCacheHint(renderPlot({ a + 1 })),
    plotCacheList
  )
  expect_equal(
    extractCacheHint(renderPlot(my_quo, quoted = TRUE)),
    plotCacheList
  )
  expect_equal(
    extractCacheHint(inject(renderPlot(!!my_quo))),
    plotCacheList
  )

  # reactive
  # Regular expression, quoted quosure object, injected quosure object
  expect_equal(
    extractCacheHint(reactive(a + 1)),
    reactiveCacheList
  )
  expect_equal(
    extractCacheHint(reactive(my_quo, quoted = TRUE)),
    reactiveCacheList
  )
  expect_equal(
    extractCacheHint(inject(reactive(!!my_quo))),
    reactiveCacheList
  )

  # markRenderFunction handles raw quosure objects as cacheHint
  expect_equal(
    extractCacheHint(
      markRenderFunction(force, force, cacheHint = list(q = my_quo))
    ),
    quoCacheList
  )
})
