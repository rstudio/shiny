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
