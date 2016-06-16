context("save-state")

test_that("decoding state query string", {
  rc <- RestoreContext$new("?a=1&b=2")
  expect_identical(rc$input$asList(), list(a=1L, b=2L))
  expect_identical(rc$values, list())

  rc <- RestoreContext$new("?a=1&b=2&_values_&c=3")
  expect_identical(rc$input$asList(), list(a=1L, b=2L))
  expect_identical(rc$values, list(c=3L))

  rc <- RestoreContext$new("?_values_&c=3")
  expect_identical(rc$input$asList(), list())
  expect_identical(rc$values, list(c=3L))

  rc <- RestoreContext$new("?a=1&b=2&_values_")
  expect_identical(rc$input$asList(), list(a=1L, b=2L))
  expect_identical(rc$values, list())

  rc <- RestoreContext$new("?_values_")
  expect_identical(rc$input$asList(), list())
  expect_identical(rc$values, list())
})
