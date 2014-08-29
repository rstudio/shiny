context("Stack")

test_that("Basic operations", {
  s <- Stack$new()
  expect_identical(s$size(), 0L)

  s$push(5)$push(6)$push(NULL)$push(list(a=1,b=2))

  expect_identical(s$pop(), list(a=1,b=2))
  expect_identical(s$peek(), NULL)
  expect_identical(s$pop(), NULL)
  expect_identical(s$size(), 2L)

  # as_list() returns in the order that they were inserted
  expect_identical(s$as_list(), list(5, 6))
})


test_that("Pushing multiple", {
  s <- Stack$new()
  s$push(1,2,3)
  s$push(4,5, .list=list(6,list(7,8)))
  s$push(9,10)
  expect_identical(s$as_list(), list(1,2,3,4,5,6,list(7,8),9,10))
  expect_identical(s$pop(), 10)
  expect_identical(s$pop(), 9)
  expect_identical(s$pop(), list(7,8))
})


test_that("Popping from empty stack", {
  s <- Stack$new()
  expect_null(s$pop())
  expect_null(s$pop())
  expect_null(s$peek())
  expect_identical(s$size(), 0L)

  s$push(5)$push(6)
  expect_identical(s$as_list(), list(5, 6))
})
