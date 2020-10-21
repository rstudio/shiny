
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


test_that("cachedReactive - valueExpr is isolated", {
  # The valueExpr is isolated; the cacheKeyExpr is the one that dependencies
  # are taken on.
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

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
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
  # If the valueExpr returns a promise, it must return a promise every time,
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

  # Initially, the `then` in the valueExpr and observer don't run, but they will
  # after running the event loop.
  flushReact()
  expect_identical(vals, c("0k", "0v1"))
  for (i in 1:3) later::run_now()
  expect_identical(vals, c("0k", "0v1", "0v2", "0o"))

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  vals <- character()
  k(1)
  flushReact()
  expect_identical(vals, c("1k", "1v1"))
  for (i in 1:3) later::run_now()
  expect_identical(vals, c("1k", "1v1", "1v2", "1o"))

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
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
  # If the valueExpr returns a promise, it must return a promise every time,
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
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0v1", "0v2", "0o"))

  # If we change k, we should see same pattern as above.
  vals <- character(0)
  k(1)
  flushReact()
  expect_identical(vals, c("1k1"))
  for (i in 1:6) later::run_now()
  expect_identical(vals, c("1k1", "1k2", "1v1", "1v2", "1o"))

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
  # the observer will.
  vals <- character(0)
  k(0)
  flushReact()
  expect_identical(vals, c("0k1"))
  for (i in 1:4) later::run_now()
  expect_identical(vals, c("0k1", "0k2", "0o"))
})

test_that("cachedReactive key collisions", {
  cache <- memoryCache()
  k <- reactiveVal(0)

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

  # The key for r2() collides with the one from r1(), so the valueExpr for r2()
  # never actually executes, and it returns the cached value for r1().
  flushReact()
  expect_identical(r1_vals, 0)
  expect_identical(r2_vals, numeric())
  expect_identical(o_vals, c(0, 0))

  k(1)
  flushReact()
  expect_identical(r1_vals, c(0, 10))
  expect_identical(r2_vals, numeric())
  expect_identical(o_vals, c(0, 0, 10, 10))
})


# ============================================================================
# Error handling
# ============================================================================
test_that("cachedReactive error handling", {
  cache <- memoryCache()

  k <- reactiveVal(0)

  # Error in key
  r_vals <- numeric()
  r <- cachedReactive(
    { k(); stop("foo") },
    {
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  suppress_stacktrace(expect_warning(flushReact()))
  # A second flushReact should not raise warnings, since cacheKeyExpr has not
  # been invalidated.
  expect_silent(flushReact())

  k(1)
  suppress_stacktrace(expect_warning(flushReact()))
  k(0)
  suppress_stacktrace(expect_warning(flushReact()))
  # valueExpr and observer shouldn't have changed at all
  expect_identical(r_vals,  numeric())
  expect_identical(o_vals,  numeric())

  # ===================================
  # Silent error in key with req(FALSE)
  cache <- memoryCache()
  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(
    { k(); req(FALSE) },
    {
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  expect_silent(flushReact())
  k(1)
  expect_silent(flushReact())
  k(0)
  expect_silent(flushReact())
  # valueExpr and observer shouldn't have changed at all
  expect_identical(r_vals,  numeric())
  expect_identical(o_vals,  numeric())

  # ===================================
  # Error in value
  cache <- memoryCache()
  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(
    { k() },
    {
      stop("foo")
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  suppress_stacktrace(expect_warning(flushReact()))
  k(1)
  suppress_stacktrace(expect_warning(flushReact()))
  k(0)
  suppress_stacktrace(expect_warning(flushReact()))
  # valueExpr and observer shouldn't have changed at all
  expect_identical(r_vals,  numeric())
  expect_identical(o_vals,  numeric())

  # =====================================
  # Silent error in value with req(FALSE)
  cache <- memoryCache()
  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(
    { k() },
    {
      req(FALSE)
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  expect_silent(flushReact())
  k(1)
  expect_silent(flushReact())
  k(0)
  expect_silent(flushReact())
  # valueExpr and observer shouldn't have changed at all
  expect_identical(r_vals,  numeric())
  expect_identical(o_vals,  numeric())
})
