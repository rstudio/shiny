
test_that("cachedReactive basic functionality", {
  cache <- memoryCache()

  k <- reactiveVal(0)

  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    },
    {
      x <- paste0(k(), "v")
      vals <<- c(vals, x)
      k()
    },
    cache = cache
  )

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


test_that("cachedReactive - value is isolated", {
  # The value is isolated; the key is the one that dependencies are taken on.
  cache <- memoryCache()

  k <- reactiveVal(1)
  v <- reactiveVal(10)

  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    },
    {
      x <- paste0(v(), "v")
      vals <<- c(vals, x)
      v()
    },
    cache = cache
  )

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
test_that("cachedReactive with async key", {
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()
  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "k1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(k(), "k2")
        vals <<- c(vals, x)
        value
      })
    },
    {
      x <- paste0(k(), "v")
      vals <<- c(vals, x)
      k()
    },
    cache = cache
  )

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

  # Going back to a cached value: The cachedReactive's value expr won't run, but
  # the observer will.
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
test_that("cachedReactives with async value", {
  # If the value expr returns a promise, it must return a promise every time,
  # even when the value is fetched in the cache. Similarly, if it returns a
  # non-promise value, then it needs to do that whether or not it's fetched from
  # the cache. This tests the promise case (almost all the other tests here test
  # the non-promise case).

  # Async value
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()

  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    },
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "v1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(value, "v2")
        vals <<- c(vals, x)
        value
      })
    },
    cache = cache
  )

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

  # Going back to a cached value: The cachedReactive's value expr won't run, but
  # the observer will.
  vals <- character()
  k(0)
  flushReact()
  expect_identical(vals, c("0k"))
  later::run_now()
  expect_identical(vals, c("0k", "0o"))
})


# ============================================================================
# Async key and value
# ============================================================================
test_that("cachedReactives with async key and value", {
  # If the value expr returns a promise, it must return a promise every time,
  # even when the value is fetched in the cache. Similarly, if it returns a
  # non-promise value, then it needs to do that whether or not it's fetched from
  # the cache. This tests the promise case (almost all the other tests here test
  # the non-promise case).

  # Async key and value
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()

  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "k1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(k(), "k2")
        vals <<- c(vals, x)
        value
      })
    },
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "v1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(value, "v2")
        vals <<- c(vals, x)
        value
      })
    },
    cache = cache
  )

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

  # Going back to a cached value: The cachedReactive's value expr won't run, but
  # the observer will.
  vals <- character(0)
  k(0)
  flushReact()
  expect_identical(vals, c("0k1"))
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0o"))
})

test_that("cachedReactive key collisions", {
  # =======================================
  # No collision with different value exprs
  # =======================================
  cache <- memoryCache()
  k <- reactiveVal(0)

  # Key collisions don't happen if they have different value expressions
  # (because that is used in the key).
  r1_vals <- numeric()
  r1 <- cachedReactive(
    k(),
    {
      val <- k() * 10
      r1_vals <<- c(r1_vals, val)
      val
    },
    cache = cache
  )

  r2_vals <- numeric()
  r2 <- cachedReactive(
    k(),
    {
      val <- k() * 100
      r2_vals <<- c(r2_vals, val)
      val
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r1(), r2())
  })

  # The key for r2() collides with the one from r1(), so the value expr for r2()
  # never actually executes, and it returns the cached value for r1().
  flushReact()
  expect_identical(r1_vals, 0)
  expect_identical(r2_vals, 0)
  expect_identical(o_vals, c(0, 0))

  k(1)
  flushReact()
  expect_identical(r1_vals, c(0, 10))
  expect_identical(r2_vals,  c(0, 100))
  expect_identical(o_vals, c(0, 0, 10, 100))


  # ====================================
  # Collision with identical value exprs
  # ====================================
  cache <- memoryCache()
  k <- reactiveVal(1)

  # Key collisions DO happen if they have different value expressions
  # (because that is used in the key).
  r_vals <- numeric()
  r1 <- cachedReactive(
    k(),
    {
      val <- k() * 10
      r_vals <<- c(r_vals, val)
      val
    },
    cache = cache
  )

  r2 <- cachedReactive(
    k(),
    {
      val <- k() * 10
      r_vals <<- c(r_vals, val)
      val
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r1(), r2())
  })

  # r2() never actually runs -- key collision.
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
test_that("cachedReactive error handling", {
  # ===================================
  # Error in key
  cache <- memoryCache()
  k <- reactiveVal(0)

  # Error in key
  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
      stop("foo")
    },
    {
      x <- paste0(k(), "v")
      k()
    },
    cache = cache
  )

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
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
      req(FALSE)
    },
    {
      x <- paste0(k(), "v")
      k()
    },
    cache = cache
  )

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
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    },
    {
      x <- paste0(k(), "v")
      vals <<- c(vals, x)
      stop("foo")
      k()
    },
    cache = cache
  )

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
  cache <- memoryCache()
  k <- reactiveVal(0)

  vals <- character()
  r <- cachedReactive(
    {
      x <- paste0(k(), "k")
      vals <<- c(vals, x)
      k()
    },
    {
      x <- paste0(k(), "v")
      vals <<- c(vals, x)
      req(FALSE)
      k()
    },
    cache = cache
  )

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


test_that("cachedReactive error handling - async", {
  # ===================================
  # Error in key
  cache <- memoryCache()
  k <- reactiveVal(0)
  vals <- character()
  r <- cachedReactive(
    {
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
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "v1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(value, "v2")
        vals <<- c(vals, x)
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
  cache <- memoryCache()
  k <- reactiveVal(0)
  vals <- character()
  r <- cachedReactive(
    {
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
    },
    {
      x <- paste0(k(), "v")
      vals <<- c(vals, x)
      resolve(k())
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
  cache <- memoryCache()
  k <- reactiveVal(0)
  vals <- character()
  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "k1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(k(), "k2")
        vals <<- c(vals, x)
        value
      })
    },
    {
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
  cache <- memoryCache()
  k <- reactiveVal(0)
  vals <- character()
  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        x <- paste0(k(), "k1")
        vals <<- c(vals, x)
        resolve(k())
      })$then(function(value) {
        x <- paste0(k(), "k2")
        vals <<- c(vals, x)
        value
      })
    },
    {
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
# Visibility
# ============================================================================
test_that("cachedReactive visibility", {
  # cachedReactive() uses rlang::as_function, and in rlang 0.4.8 and below, it
  # did not preserve visibility.
  # https://github.com/r-lib/rlang/issues/1055
  skip_if_not_installed("rlang", "0.4.8.9000")
  cache <- memoryCache()
  k <- reactiveVal(0)
  res <- NULL
  r <- cachedReactive(
    k(),
    {
      if (k() == 0) invisible(k())
      else          k()
    },
    cache = cache
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


test_that("cachedReactive visibility - async", {
  # Skippping because of https://github.com/rstudio/promises/issues/58
  skip("Visibility currently not supported by promises")
  cache <- memoryCache()
  k <- reactiveVal(0)
  res <- NULL
  r <- cachedReactive(
    k(),
    {
      promise(function(resolve, reject) {
        if (k() == 0) resolve(invisible(k()))
        else          resolve(k())
      })
    },
    cache = cache
  )

  o <- observe({
    r()$then(function(value) {
      res <<- withVisible(value)
    })
  })

  flushReact()
  for (i in 1:3) later::run_now()
  expect_identical(res, list(value = 0, visible = FALSE))
  k(1)
  flushReact()
  for (i in 1:3) later::run_now()
  expect_identical(res, list(value = 1, visible = TRUE))
  # Now fetch from cache
  k(0)
  flushReact()
  for (i in 1:3) later::run_now()
  expect_identical(res, list(value = 0, visible = FALSE))
  k(1)
  flushReact()
  for (i in 1:3) later::run_now()
  expect_identical(res, list(value = 1, visible = TRUE))
})
