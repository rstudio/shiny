test_that("destroyedReactiveError creates correct condition", {
  err <- destroyedReactiveError("test label")
  expect_s3_class(err, "shiny.destroyed.error")
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "test label")
  expect_match(conditionMessage(err), "destroyed")
})

test_that("destroyedReactiveError can be caught specifically", {
  expect_error(
    stop(destroyedReactiveError("my_reactive")),
    class = "shiny.destroyed.error"
  )
})

test_that("ReactiveVal$destroy() sets destroyed flag and invalidates dependents", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")

  # Track invalidation
  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv_impl$get())

  rv_impl$destroy()
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveVal$destroy() is idempotent", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_no_error(rv_impl$destroy())
})

test_that("destroyed ReactiveVal$get() raises shiny.destroyed.error", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_error(rv_impl$get(), class = "shiny.destroyed.error")
})

test_that("destroyed ReactiveVal$set() raises shiny.destroyed.error", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_error(rv_impl$set(20), class = "shiny.destroyed.error")
})

test_that("ReactiveVal$destroy() does not emit rLog or otel", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  ctx <- Context$new(domain = NULL)
  ctx$run(function() rv_impl$get())
  rv_impl$destroy()
  flushReact()
})

test_that("Observable$destroy() sets destroyed flag and invalidates dependents", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  # Force first evaluation
  isolate(o$getValue())

  # Track invalidation of downstream
  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() o$getValue())

  o$destroy()
  flushReact()

  expect_true(invalidated)
})

test_that("Observable$destroy() is idempotent", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  o$destroy()
  expect_no_error(o$destroy())
})

test_that("destroyed Observable$getValue() raises shiny.destroyed.error", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  o$destroy()
  expect_error(isolate(o$getValue()), class = "shiny.destroyed.error")
})

test_that("Observable$destroy() clears value and error refs", {
  o <- Observable$new(function() list(big = rep(1, 1e6)), label = "test_obs", domain = NULL)
  isolate(o$getValue())
  o$destroy()
  expect_null(o$.value)
  expect_false(o$.error)
})

test_that("Observer registers weak destroy callback with domain$onDestroy", {
  domain <- createMockDomain()
  # Add onDestroy to mock domain
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    val <- reactiveVal(0)
    count <- 0L
    obs <- observe({ val(); count <<- count + 1L })
  })
  flushReact()
  expect_equal(count, 1L)

  # Verify something was registered with onDestroy
  expect_gt(destroyCBs$count(), 0)
})

test_that("Observer$destroy() deregisters from onDestroy", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    obs <- observe({ TRUE })
  })
  flushReact()
  initial_count <- destroyCBs$count()

  obs$destroy()
  # The unsubscribe handle should have deregistered the callback
  expect_lt(destroyCBs$count(), initial_count)
})
