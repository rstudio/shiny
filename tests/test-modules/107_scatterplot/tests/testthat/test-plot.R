
context("plot")

test_that("Value exists", {
  expect_true(exists("alpha_val"))
  expect_equal(alpha_val, 0.2)
})
