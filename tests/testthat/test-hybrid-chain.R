test_that("hybrid_chain preserves visibility", {
  expect_identical(
    hybrid_chain(1, withVisible),
    list(value = 1, visible = TRUE)
  )

  expect_identical(
    hybrid_chain(invisible(1), withVisible),
    list(value = 1, visible = FALSE)
  )

  expect_identical(
    hybrid_chain(1, identity, withVisible),
    list(value = 1, visible = TRUE)
  )

  expect_identical(
    hybrid_chain(invisible(1), identity, withVisible),
    list(value = 1, visible = FALSE)
  )

  expect_identical(
    hybrid_chain(1, function(x) invisible(x), withVisible),
    list(value = 1, visible = FALSE)
  )
})


test_that("hybrid_chain preserves visibility - async", {
  # only test if promises handles visibility
  skip_if_not_installed("promises", "1.1.1.9001")

  res <- NULL
  hybrid_chain(
    promise_resolve(1),
    function(value) res <<- withVisible(value)
  )
  later::run_now()
  expect_identical(res, list(value = 1, visible = TRUE))

  res <- NULL
  hybrid_chain(
    promise_resolve(invisible(1)),
    function(value) res <<- withVisible(value)
  )
  later::run_now()
  expect_identical(res, list(value = 1, visible = FALSE))

  res <- NULL
  hybrid_chain(
    promise_resolve(1),
    identity,
    function(value) res <<- withVisible(value)
  )
  for (i in 1:2) later::run_now()
  expect_identical(res, list(value = 1, visible = TRUE))

  res <- NULL
  hybrid_chain(
    promise_resolve(invisible(1)),
    identity,
    function(value) res <<- withVisible(value)
  )
  for (i in 1:2) later::run_now()
  expect_identical(res, list(value = 1, visible = FALSE))

  res <- NULL
  hybrid_chain(
    promise_resolve(1),
    function(x) invisible(x),
    function(value) res <<- withVisible(value)
  )
  for (i in 1:2) later::run_now()
  expect_identical(res, list(value = 1, visible = FALSE))


})
