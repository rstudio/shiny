context("UI")


# For issue #1006
test_that("sliderInput steps don't have rounding errors", {
  # Need to use expect_identical; expect_equal is too forgiving of rounding error
  expect_identical(findStepSize(-5.5, 4, NULL), 0.1)
})
