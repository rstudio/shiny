context("bookmarking")

test_that("Inputs and values in query string", {
  # Normal format
  vals <- RestoreContext$new("?_inputs_&a=1&b=2&_values_&x=3")$asList()
  expect_true(contents_identical(vals$input, list(a=1L, b=2L)))
  expect_identical(as.list(vals$values), list(x=3L))

  # No leading '?', trailing '&', and values before inputs
  vals <- RestoreContext$new("_values_&x=3&_inputs_&a=1&b=2&")$asList()
  expect_true(contents_identical(vals$input, list(a=1L, b=2L)))
  expect_identical(as.list(vals$values), list(x=3L))

  # Just inputs, no values, and leading '&'
  vals <- RestoreContext$new("&_inputs_&a=1&b=2")$asList()
  expect_true(contents_identical(vals$input, list(a=1L, b=2L)))
  expect_identical(as.list(vals$values), list())

  # No inputs, just values
  vals <- RestoreContext$new("?_values_&x=3")$asList()
  expect_identical(vals$input, list())
  expect_identical(as.list(vals$values), list(x=3L))

  # Empty query string
  vals <- RestoreContext$new("")$asList()
  expect_identical(vals$input, list())
  expect_identical(as.list(vals$values), list())

  # Other items (not inputs and not values)
  vals <- RestoreContext$new("?c=3&d=4")$asList()
  expect_identical(vals$input, list())
  expect_identical(as.list(vals$values), list())

  # Multiple instances of _inputs_ or _values_
  expect_warning(suppressMessages(RestoreContext$new("?_inputs_&a=1&_inputs_")))
  expect_warning(suppressMessages(RestoreContext$new("?_inputs_&a=1&_inputs_&")))
  expect_warning(suppressMessages(RestoreContext$new("?_inputs_&a=1&_inputs_&b=2")))
  expect_warning(suppressMessages(RestoreContext$new("?_inputs_&a=1&_values_&b=2&_inputs_&")))
  expect_warning(suppressMessages(RestoreContext$new("?_values_&a=1&_values_")))
  expect_warning(suppressMessages(RestoreContext$new("?_inputs_&a=1&_values_&_values&b=2")))

  # If there's an error in the conversion from query string, should have
  # blank values.
  expect_warning(suppressMessages(rc <- RestoreContext$new("?_inputs_&a=[x&b=1")))
  expect_identical(rc$input$asList(), list())
  expect_identical(as.list(rc$values), list())
  expect_identical(rc$dir, NULL)

  # Ignore query string if it's a subapp
  rc <- RestoreContext$new("?w=&__subapp__=1")
  expect_identical(rc$input$asList(), list())
  expect_identical(as.list(rc$values), list())
  expect_identical(rc$dir, NULL)
})
