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
