# Test the lexical_sort function from R/example.R
context("sort")

test_that("Lexical sorting works", {
  expect_equal(lexical_sort(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(lexical_sort(c(1, 2, 3, 13, 11, 21)), c(1, 11, 13, 2, 21, 3))
})
