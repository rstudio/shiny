
test_that("cachedReactive basic functionality", {
  cache <- memoryCache()

  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(k(), {
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  flushReact()

  k(1)
  flushReact()
  k(2)
  flushReact()
  expect_identical(r_vals, c(0, 1, 2))
  expect_identical(o_vals, c(0, 1, 2))

  # Use a value that is in the cache. o will re-execute, but r will not.
  k(1)
  flushReact()
  expect_identical(r_vals, c(0, 1, 2))
  expect_identical(o_vals, c(0, 1, 2, 1))
  k(0)
  flushReact()
  expect_identical(r_vals, c(0, 1, 2))
  expect_identical(o_vals, c(0, 1, 2, 1, 0))

  # Reset the cache - r will re-execute even if it's a previously-used value.
  cache$reset()
  k(1)
  flushReact()
  expect_identical(r_vals, c(0, 1, 2, 1))
  expect_identical(o_vals, c(0, 1, 2, 1, 0, 1))
})


test_that("cachedReactive - valueExpr is isolated", {
  # The valueExpr is isolated; the cacheKeyExpr is the one that dependencies
  # are taken on.
  cache <- memoryCache()

  k <- reactiveVal(1)
  v <- reactiveVal(10)

  r_vals <- numeric()
  r <- cachedReactive(k(), {
      r_vals <<- c(r_vals, v())
      v()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    o_vals <<- c(o_vals, r())
  })

  flushReact()

  # Changing v() triggers reactivity
  k(2)
  flushReact()
  k(3)
  flushReact()
  expect_identical(r_vals, c(10, 10, 10))
  expect_identical(o_vals, c(10, 10, 10))

  # Changing w() does not trigger reactivity
  v(20)
  flushReact()
  v(30)
  flushReact()
  expect_identical(r_vals, c(10, 10, 10))
  expect_identical(o_vals, c(10, 10, 10))

  # If v() changes, it will invalidate r, which will invalidate o. r will not
  # re-execute, but instead fetch the old value (10) from the cache (from when
  # the key was 1), and that value will be passed to o. This is an example of a
  # bad key!
  k(1)
  flushReact()
  expect_identical(r_vals, c(10, 10, 10))
  expect_identical(o_vals, c(10, 10, 10, 10))

  # A new un-cached value for v will cause r to re-execute; it will fetch the
  # current value of w (30), and that value will be passed to o.
  k(4)
  flushReact()
  expect_identical(r_vals, c(10, 10, 10, 30))
  expect_identical(o_vals, c(10, 10, 10, 10, 30))
})


# ============================================================================
# Async key
# ============================================================================
test_that("cachedReactive with async key", {
  cache <- memoryCache()
  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        resolve(k())
      })
    },
    {
      r_vals <<- c(r_vals, k())
      k()
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    # v <- r()
    # o_vals <<- c(o_vals, v)
    r()$then(function(value) {
      o_vals <<- c(o_vals, value)
    })
  })

  # Initially, the valueExpr and observer don't run; they will run on the next
  # tick of the event loop.
  flushReact()
  expect_identical(r_vals, numeric())
  expect_identical(o_vals, numeric())
  # After running the event loop once, the cachedReactive's valueExpr will run,
  # but the observer's then() function won't run yet (it gets scheduled in this
  # tick).
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # One more run of the event loop, and the then() function will run.
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, 0)

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  k(1)
  flushReact()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
  # the observer will.
  k(0)
  flushReact()
  # Step 1: cacheKeyExpr runs
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # Step 2: value is retrieved from the cache (valueExpr doesn't run)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # Step 3: observer's then() function runs.
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1, 0))
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

  r_vals <- numeric()
  r <- cachedReactive(
    k(),
    {
      promises::promise(function(resolve, reject) {
        r_vals <<- c(r_vals, k())
        resolve(k())
      })
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    p <- r()
    p$then(function(value) {
      o_vals <<- c(o_vals, value)
    })
  })

  # Initially, the valueExpr and observer don't run; they will run on the next
  # tick of the event loop.
  flushReact()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # After running the event loop once, the cachedReactive's valueExpr will run,
  # but the observer's then() function won't run yet (it gets scheduled in this
  # tick).
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # One more run of the event loop, and the then() function will run.
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, 0)

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  k(1)
  flushReact()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
  # the observer will.
  k(0)
  flushReact()
  # Step 1: cacheKeyExpr runs, value is retrieved from the cache, wrapped in a
  # promise, and returned.
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # Step 2: observer's then() function runs.
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1, 0))
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

  # Async value
  cache <- memoryCache()
  k <- reactiveVal(0)

  r_vals <- numeric()
  r <- cachedReactive(
    {
      promises::promise(function(resolve, reject) {
        resolve(k())
      })
    },
    {
      promises::promise(function(resolve, reject) {
        r_vals <<- c(r_vals, k())
        resolve(k())
      })
    },
    cache = cache
  )

  o_vals <- numeric()
  o <- observe({
    p <- r()
    p$then(function(value) {
      o_vals <<- c(o_vals, value)
    })
  })

  # Initially, the valueExpr and observer don't run; they will run on the next
  # tick of the event loop.
  flushReact()
  expect_identical(r_vals, numeric())
  expect_identical(o_vals, numeric())
  # After running the event loop once, the cachedReactive's valueExpr will run,
  # but the observer's then() function won't run yet (it gets scheduled in this
  # tick).
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # Next tick: the then() in the cachedReactive sets the value in the cache, and
  # then returns the value.
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # Next tick: ???
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, numeric())
  # Next tick: The then() function in the observer will run.
  later::run_now()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, 0)

  # If we change k, we should see same pattern as above, where run_now() is
  # needed for the promise callbacks to run.
  k(1)
  flushReact()
  expect_identical(r_vals, 0)
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, 0)
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))

  # Going back to a cached value: The cachedReactive's valueExpr won't run, but
  # the observer will.
  k(0)
  flushReact()
  # Step 1: cacheKeyExpr promise runs
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # The next step in the hybrid_chain runs, value is retrieved from the cache,
  # wrapped in a promise, and the promise is returned.
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # ???
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1))
  # Observer's then() function runs.
  later::run_now()
  expect_identical(r_vals, c(0, 1))
  expect_identical(o_vals, c(0, 1, 0))
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

  rm(list = ls()); gc()
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

  rm(list = ls()); gc()
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

  rm(list = ls()); gc()
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
