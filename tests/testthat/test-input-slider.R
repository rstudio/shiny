# For issue #1006
test_that("sliderInput steps don't have rounding errors", {
  # Need to use expect_identical; expect_equal is too forgiving of rounding error
  expect_identical(findStepSize(-5.5, 4, NULL), 0.1)
})

test_that("sliderInput can use numbers, dates, or POSIXct", {
  n <- 1
  d <- Sys.Date()
  dt <- Sys.time()

  expect_error(sliderInput("x", "x", n - 1, n + 1, n), NA)
  expect_error(sliderInput("x", "x", d - 1, d + 1, d), NA)
  expect_error(sliderInput("x", "x", dt - 1, dt + 1, dt), NA)
})

test_that("sliderInput gives informative errors for bad inputs", {
  expect_snapshot(error = TRUE, {
    sliderInput("x", "x")
    sliderInput("x", "x", min = NULL, max = 3, value = 2)
    sliderInput("x", "x", min = 1, max = NULL, value = 2)
    sliderInput("x", "x", min = 1, max = 3, value = NULL)
    sliderInput("x", "x", min = Sys.Date(), max = Sys.Date(), value = 1)
    sliderInput("x", "x", min = 1, max = 3, value = 0)
  })
})
