
# Note that there are some tests for bindEvent() and caching in
# test-with-cache.R.

test_that("bindEvent and observers", {
  trigger <- reactiveVal(1)
  val <- reactiveVal(10)

  vals <- numeric()
  o <- bindEvent(
    trigger(),
    x = observe({
      vals <<- c(vals, val())
    })
  )

  flushReact()
  expect_identical(vals, 10)

  # Changing val has no effect
  val(20)
  flushReact()
  expect_identical(vals, 10)

  # Changing trigger causes the observer to execute
  trigger(2)
  flushReact()
  expect_identical(vals, c(10, 20))

  trigger(3)
  flushReact()
  expect_identical(vals, c(10, 20, 20))
})


test_that("bindEvent does not prevent observers from being GC'd", {
  v <- reactiveVal(1)
  o <- observe({ message(v()) })

  finalized <- FALSE
  reg.finalizer(o, function(e) { finalized <<- TRUE })

  # o1 shouldn't keep a reference to o (and prevent it from getting GC'd).
  o1 <- bindEvent(o, v())
  rm(o)

  flushReact()
  gc()
  expect_true(finalized)
})


test_that("ignoreNULL works", {
  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, ignoreNULL = FALSE)
  flushReact()
  expect_identical(n, 1)

  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, ignoreNULL = TRUE)
  flushReact()
  expect_identical(n, 0)

  # Two NULLs in the `...` get aggregated into a list, so the result is not
  # NULL.
  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, NULL, ignoreNULL = TRUE)
  flushReact()
  expect_identical(n, 1)
})
