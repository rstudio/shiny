## Helper functions

## Creates a counting reactive function, that keeps track of how many times it
## is called. You call it using foo$call() and get the count with foo$times.
creactive <- function(func) {
  env <- new.env()
  env$times <- 0
  wrappedFunc <- reactive(function() {
    env$times <- env$times + 1
    func()
  })
  env$call <- wrappedFunc
  env
}

cobserve <- function(func) {
  env <- new.env()
  env$times <- 0
  observe(function() {
    env$times <- env$times + 1
    func()
  })
  env
}


# Test for overreactivity. funcB has an indirect dependency on valueA (via
# funcA) and also a direct dependency on valueA. When valueA changes, funcB
# should only execute once.
test_that("Functions are not over-reactive", {

  valueA <- reactiveValue(10)

  funcA <- creactive(function() {
    value(valueA)
  })

  funcB <- creactive(function() {
    funcA$call()
    value(valueA)
  })

  obsC <- cobserve(function() {
    funcB$call()
  })

  flushReact()
  expect_equal(funcB$times, 1)
  expect_equal(obsC$times, 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(funcB$times, 2)
  expect_equal(obsC$times, 2)
})


## Test for isolation. funcB depends on funcA depends on valueA. When funcA
## is invalidated, if its new result is not different than its old result,
## then it doesn't invalidate its dependents. This is done by adding an observer
## (valueB) between obsA and funcC.
test_that("isolation", {
  valueA <- reactiveValue(10)
  valueB <- reactiveValue(NULL)

  obsA <- cobserve(function() {
    value(valueB) <- value(valueA) > 0
  })

  funcC <- creactive(function() {
    value(valueB)
  })

  obsB <- cobserve(function() {
    funcC$call()
  })

  flushReact()
  expect_equal(funcC$times, 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(funcC$times, 1)
})


## Test for laziness. With lazy evaluation, the observers should "pull" values
## from their dependent functions. In contrast, eager evaluation would have
## reactive values and functions "push" their changes down to their descendents.
test_that("laziness", {

  valueA <- reactiveValue(10)

  funcA <- creactive(function() {
    value(valueA) > 0
  })

  funcB <- creactive(function() {
    funcA$call()
  })

  obsC <- cobserve(function() {
    if (value(valueA) > 10)
      return()
    funcB$call()
  })

  flushReact()
  expect_equal(funcA$times, 1)
  expect_equal(funcB$times, 1)
  expect_equal(obsC$times, 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(funcA$times, 1)
  expect_equal(funcB$times, 1)
  expect_equal(obsC$times, 2)
})
