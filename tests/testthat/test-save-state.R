context("save-state")

test_that("decoding state query string", {
  expect_identical(
    decodeStateQueryString("?a=1&b=2"),
    list(input = list(a=1L, b=2L), values = list())
  )
  expect_identical(
    decodeStateQueryString("?a=1&b=2&_values_&c=3"),
    list(input = list(a=1L, b=2L), values = list(c=3L))
  )
  expect_identical(
    decodeStateQueryString("?_values_&c=3"),
    list(input = list(), values = list(c=3L))
  )
  expect_identical(
    decodeStateQueryString("?a=1&b=2&_values_"),
    list(input = list(a=1L, b=2L), values = list())
  )
  expect_identical(
    decodeStateQueryString("?_values_"),
    list(input = list(), values = list())
  )
})
