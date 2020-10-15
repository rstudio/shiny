
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
