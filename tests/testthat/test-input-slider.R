# For issue #1006
test_that("sliderInput steps don't have rounding errors", {
  # Need to use expect_identical; expect_equal is too forgiving of rounding error
  expect_identical(findStepSize(-5.5, 4, NULL), 0.1)
})



test_that("sliderInput validation", {
  # Number
  x <- 10
  expect_silent(sliderInput('s', 's', x-1, x+1, x))
  expect_silent(sliderInput('s', 's', x-1, x+1, x-1))
  expect_silent(sliderInput('s', 's', x-1, x+1, c(x-1, x+1)))

  expect_warning(sliderInput('s', 's', x-1,  x+1,  x+2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  x-2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x-2, x)))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x, x+2)))

  expect_error(sliderInput('s', 's', x-1,  x+1))
  expect_error(sliderInput('s', 's', x-1,  x+1,  NULL))
  expect_error(sliderInput('s', 's', x-1,  NULL, x))
  expect_error(sliderInput('s', 's', NULL, x+1,  x))
  expect_error(sliderInput('s', 's', NULL, NULL, x))
  expect_error(sliderInput('s', 's', x-1,  x+1, NA_real_))
  expect_error(sliderInput('s', 's', x-1,  x+1, c(x, NA_real_)))

  # Date
  x <- Sys.Date()
  expect_silent(sliderInput('s', 's', x-1, x+1, x))
  expect_silent(sliderInput('s', 's', x-1, x+1, x-1))
  expect_silent(sliderInput('s', 's', x-1, x+1, c(x-1, x+1)))

  expect_warning(sliderInput('s', 's', x-1,  x+1,  x+2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  x-2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x-2, x)))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x, x+2)))

  expect_error(sliderInput('s', 's', x-1,  x+1))
  expect_error(sliderInput('s', 's', x-1,  x+1,  NULL))
  expect_error(sliderInput('s', 's', x-1,  NULL, x))
  expect_error(sliderInput('s', 's', NULL, x+1,  x))
  expect_error(sliderInput('s', 's', NULL, NULL, x))
  expect_error(sliderInput('s', 's', x-1,  x+1, as.Date(NA)))

  # POSIXct
  x <- Sys.time()
  expect_silent(sliderInput('s', 's', x-1, x+1, x))
  expect_silent(sliderInput('s', 's', x-1, x+1, x-1))
  expect_silent(sliderInput('s', 's', x-1, x+1, c(x-1, x+1)))

  expect_warning(sliderInput('s', 's', x-1,  x+1,  x+2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  x-2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x-2, x)))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x, x+2)))

  expect_error(sliderInput('s', 's', x-1,  x+1))
  expect_error(sliderInput('s', 's', x-1,  x+1,  NULL))
  expect_error(sliderInput('s', 's', x-1,  NULL, x))
  expect_error(sliderInput('s', 's', NULL, x+1,  x))
  expect_error(sliderInput('s', 's', NULL, NULL, x))

  # POSIXLt
  x <- as.POSIXlt(Sys.time())
  expect_silent(sliderInput('s', 's', x-1, x+1, x))
  expect_silent(sliderInput('s', 's', x-1, x+1, x-1))

  expect_warning(sliderInput('s', 's', x-1,  x+1,  x+2))
  expect_warning(sliderInput('s', 's', x-1,  x+1,  x-2))

  if (getRversion() >= "4.0") {
    expect_silent(sliderInput('s', 's', x-1, x+1, c(x-1, x+1)))
    expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x-2, x)))
    expect_warning(sliderInput('s', 's', x-1,  x+1,  c(x, x+2)))
  } else {
    skip("c() doesn't work sensibly on POSIXlt objects with this version of R")
  }


  expect_error(sliderInput('s', 's', x-1,  x+1))
  expect_error(sliderInput('s', 's', x-1,  x+1,  NULL))
  expect_error(sliderInput('s', 's', x-1,  NULL, x))
  expect_error(sliderInput('s', 's', NULL, x+1,  x))
  expect_error(sliderInput('s', 's', NULL, NULL, x))


  # Size
  x <- 10
  ## length 0
  expect_error(sliderInput('s', 's', x-1, x+1, numeric(0)))
  expect_error(sliderInput('s', 's', x-1, numeric(0), x))
  expect_error(sliderInput('s', 's', numeric(0), x+1, x))
  ## length 1
  expect_silent(sliderInput('s', 's', x-1, x+1, x))
  ## length 2
  expect_silent(sliderInput('s', 's', x-1, x+1, c(x, x)))
  ## length 3+
  expect_error(sliderInput('s', 's', x-1, x+1, c(x, x, x)))
  expect_error(sliderInput('s', 's', x-1, c(x, x, x), x))
  expect_error(sliderInput('s', 's', c(x, x, x), x+1, x))
})
