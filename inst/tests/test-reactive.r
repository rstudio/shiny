## Helper functions

# Test for overreactivity. funcB has an indirect dependency on valueA (via
# funcA) and also a direct dependency on valueA. When valueA changes, funcB
# should only execute once.
test_that("Functions are not over-reactive", {

  valueA <- reactiveValue(10)

  funcA <- reactive(function() {
    value(valueA)
  })

  funcB <- reactive(function() {
    funcA()
    value(valueA)
  })

  obsC <- observe(function() {
    funcB()
  })

  flushReact()
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)
})


## Test for isolation. funcB depends on funcA depends on valueA. When funcA
## is invalidated, if its new result is not different than its old result,
## then it doesn't invalidate its dependents. This is done by adding an observer
## (valueB) between obsA and funcC.
test_that("isolation", {
  valueA <- reactiveValue(10)
  valueB <- reactiveValue(NULL)

  obsA <- observe(function() {
    value(valueB) <- value(valueA) > 0
  })

  funcC <- reactive(function() {
    value(valueB)
  })

  obsB <- observe(function() {
    funcC()
  })

  flushReact()
  expect_equal(execCount(funcC), 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(execCount(funcC), 1)
})


## Test for laziness. With lazy evaluation, the observers should "pull" values
## from their dependent functions. In contrast, eager evaluation would have
## reactive values and functions "push" their changes down to their descendents.
test_that("laziness", {

  valueA <- reactiveValue(10)

  funcA <- reactive(function() {
    value(valueA) > 0
  })

  funcB <- reactive(function() {
    funcA()
  })

  obsC <- observe(function() {
    if (value(valueA) > 10)
      return()
    funcB()
  })

  flushReact()
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  value(valueA) <- 11
  flushReact()
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 2)
})


## Suppose B depends on A and C depends on A and B. Then when A is changed,
## the evaluation order should be A, B, C. Also, each time A is changed, B and
## C should be run once, if we want to be maximally efficient.
test_that("order of evaluation", {
  # ----------------------------------------------
  # Test 1
  # B depends on A, and observer depends on A and B. The observer uses A and
  # B, in that order.

  # This is to store the value from observe()
  observed_value <- NA

  valueA <- reactiveValue(1)
  funcB  <- reactive(function() {
    value(valueA) + 5
  })
  obsC <- observe(function() {
    observed_value <<- value(valueA) * funcB()
  })

  flushReact()
  expect_equal(observed_value, 6)   # Should be 1 * (1 + 5) = 6
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  value(valueA) <- 2
  flushReact()
  expect_equal(observed_value, 14)  # Should be 2 * (2 + 5) = 14
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)


  # ----------------------------------------------
  # Test 2:
  # Same as Test 1, except the observer uses A and B in reversed order.
  # Resulting values should be the same.

  observed_value <- NA

  valueA <- reactiveValue(1)
  funcB <- reactive(function() {
    value(valueA) + 5
  })
  obsC <- observe(function() {
    observed_value <<- funcB() * value(valueA)
  })

  flushReact()
  # Should be 1 * (1 + 5) = 6
  expect_equal(observed_value, 6)
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  value(valueA) <- 2
  flushReact()
  # Should be 2 * (2 + 5) = 14
  expect_equal(observed_value, 14)
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)
})


## Expressions in isolate() should not invalidate the parent context.
test_that("isolate() blocks invalidations from propagating", {

  obsC_value <- NA
  obsD_value <- NA

  valueA <- reactiveValue(1)
  valueB <- reactiveValue(10)
  funcB <- reactive(function() {
    value(valueB) + 100
  })

  # References to valueB and funcB are isolated
  obsC <- observe(function() {
    obsC_value <<-
      value(valueA) + isolate(value(valueB)) + isolate(funcB())
  })

  # In contrast with obsC, this has a non-isolated reference to funcB
  obsD <- observe(function() {
    obsD_value <<-
      value(valueA) + isolate(value(valueB)) + funcB()
  })


  flushReact()
  expect_equal(obsC_value, 121)
  expect_equal(execCount(obsC), 1)
  expect_equal(obsD_value, 121)
  expect_equal(execCount(obsD), 1)

  # Changing A should invalidate obsC and obsD
  value(valueA) <- 2
  flushReact()
  expect_equal(obsC_value, 122)
  expect_equal(execCount(obsC), 2)
  expect_equal(obsD_value, 122)
  expect_equal(execCount(obsD), 2)

  # Changing B shouldn't invalidate obsC becuause references to B are in isolate()
  # But it should invalidate obsD.
  value(valueB) <- 20
  flushReact()
  expect_equal(obsC_value, 122)
  expect_equal(execCount(obsC), 2)
  expect_equal(obsD_value, 142)
  expect_equal(execCount(obsD), 3)

  # Changing A should invalidate obsC and obsD, and they should see updated
  # values for valueA, valueB, and funcB
  value(valueA) <- 3
  flushReact()
  expect_equal(obsC_value, 143)
  expect_equal(execCount(obsC), 3)
  expect_equal(obsD_value, 143)
  expect_equal(execCount(obsD), 4)
})
