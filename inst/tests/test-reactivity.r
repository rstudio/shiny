context("reactivity")


# Test for correct behavior of ReactiveValues
test_that("ReactiveValues", {
  # Creation and indexing into ReactiveValues -------------------------------
  values <- reactiveValues()

  # $ indexing
  values$a <- 3
  expect_equal(isolate(values$a), 3)

  # [[ indexing
  values[['a']] <- 4
  expect_equal(isolate(values[['a']]), 4)

  # Create with initialized values
  values <- reactiveValues(a=1, b=2)
  expect_equal(isolate(values$a), 1)
  expect_equal(isolate(values[['b']]), 2)

  # NULL values -------------------------------------------------------------
  # Initializing with NULL value
  values <- reactiveValues(a=NULL, b=2)
  # a should exist and be NULL
  expect_equal(isolate(names(values)), c("a", "b"))
  expect_true(is.null(isolate(values$a)))

  # Assigning NULL should keep object (not delete it), and set value to NULL
  values$b <- NULL
  expect_equal(isolate(names(values)), c("a", "b"))
  expect_true(is.null(isolate(values$b)))


  # Errors -----------------------------------------------------------------
  # Error: indexing with non-string
  expect_error(isolate(values[[1]]))
  expect_error(isolate(values[[NULL]]))
  expect_error(isolate(values[[list('a')]]))

  # Error: [ indexing shouldn't work
  expect_error(isolate(values['a']))
  expect_error(isolate(values['a'] <- 1))

  # Error: unnamed arguments
  expect_error(reactiveValues(1, b=2))

  # Error: assignment to readonly values
  values <- .create_reactivevalues(ReactiveValues$new(), readonly = TRUE)
  expect_error(values$a <- 1)
})


# Test for overreactivity. funcB has an indirect dependency on valueA (via
# funcA) and also a direct dependency on valueA. When valueA changes, funcB
# should only execute once.
test_that("Functions are not over-reactive", {

  values <- reactiveValues(A=10)

  funcA <- reactive(function() {
    values$A
  })

  funcB <- reactive(function() {
    funcA()
    values$A
  })

  obsC <- observe(function() {
    funcB()
  })

  flushReact()
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  values$A <- 11
  flushReact()
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)
})

## "foo => bar" is defined as "foo is a dependency of bar"
##
## vA => fB
## (fB, vA) => obsE
## (fB, vA) => obsF
##
## obsE and obsF should each execute once when vA changes.
test_that("overreactivity2", {
  # ----------------------------------------------
  # Test 1
  # B depends on A, and observer depends on A and B. The observer uses A and
  # B, in that order.

  # This is to store the value from observe()
  observed_value1 <- NA
  observed_value2 <- NA

  values <- reactiveValues(A=1)
  funcB  <- reactive(function() {
    values$A + 5
  })
  obsC <- observe(function() {
    observed_value1 <<-  funcB() * values$A
  })
  obsD <- observe(function() {
    observed_value2 <<-  funcB() * values$A
  })

  flushReact()
  expect_equal(observed_value1, 6)   # Should be 1 * (1 + 5) = 6
  expect_equal(observed_value2, 6)   # Should be 1 * (1 + 5) = 6
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)
  expect_equal(execCount(obsD), 1)

  values$A <- 2
  flushReact()
  expect_equal(observed_value1, 14)  # Should be 2 * (2 + 5) = 14
  expect_equal(observed_value2, 14)  # Should be 2 * (2 + 5) = 14
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)
  expect_equal(execCount(obsD), 2)
})

## Test for isolation. funcB depends on funcA depends on valueA. When funcA
## is invalidated, if its new result is not different than its old result,
## then it doesn't invalidate its dependents. This is done by adding an observer
## (valueB) between obsA and funcC.
##
## valueA => obsB => valueC => funcD => obsE
test_that("isolation", {
  values <- reactiveValues(A=10, C=NULL)

  obsB <- observe(function() {
    values$C <- values$A > 0
  })

  funcD <- reactive(function() {
    values$C
  })

  obsE <- observe(function() {
    funcD()
  })

  flushReact()
  countD <- execCount(funcD)

  values$A <- 11
  flushReact()
  expect_equal(execCount(funcD), countD)
})


## Test for laziness. With lazy evaluation, the observers should "pull" values
## from their dependent functions. In contrast, eager evaluation would have
## reactive values and functions "push" their changes down to their descendents.
test_that("laziness", {

  values <- reactiveValues(A=10)

  funcA <- reactive(function() {
    values$A > 0
  })

  funcB <- reactive(function() {
    funcA()
  })

  obsC <- observe(function() {
    if (values$A > 10)
      return()
    funcB()
  })

  flushReact()
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  values$A <- 11
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

  values <- reactiveValues(A=1)
  funcB  <- reactive(function() {
    values$A + 5
  })
  obsC <- observe(function() {
    observed_value <<- values$A * funcB()
  })

  flushReact()
  expect_equal(observed_value, 6)   # Should be 1 * (1 + 5) = 6
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  values$A <- 2
  flushReact()
  expect_equal(observed_value, 14)  # Should be 2 * (2 + 5) = 14
  expect_equal(execCount(funcB), 2)
  expect_equal(execCount(obsC), 2)


  # ----------------------------------------------
  # Test 2:
  # Same as Test 1, except the observer uses A and B in reversed order.
  # Resulting values should be the same.

  observed_value <- NA

  values <- reactiveValues(A=1)
  funcB <- reactive(function() {
    values$A + 5
  })
  obsC <- observe(function() {
    observed_value <<- funcB() * values$A
  })

  flushReact()
  # Should be 1 * (1 + 5) = 6
  expect_equal(observed_value, 6)
  expect_equal(execCount(funcB), 1)
  expect_equal(execCount(obsC), 1)

  values$A <- 2
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

  values <- reactiveValues(A=1, B=10)
  funcB <- reactive(function() {
    values$B + 100
  })

  # References to valueB and funcB are isolated
  obsC <- observe(function() {
    obsC_value <<-
      values$A + isolate(values$B) + isolate(funcB())
  })

  # In contrast with obsC, this has a non-isolated reference to funcB
  obsD <- observe(function() {
    obsD_value <<-
      values$A + isolate(values$B) + funcB()
  })


  flushReact()
  expect_equal(obsC_value, 121)
  expect_equal(execCount(obsC), 1)
  expect_equal(obsD_value, 121)
  expect_equal(execCount(obsD), 1)

  # Changing A should invalidate obsC and obsD
  values$A <- 2
  flushReact()
  expect_equal(obsC_value, 122)
  expect_equal(execCount(obsC), 2)
  expect_equal(obsD_value, 122)
  expect_equal(execCount(obsD), 2)

  # Changing B shouldn't invalidate obsC becuause references to B are in isolate()
  # But it should invalidate obsD.
  values$B <- 20
  flushReact()
  expect_equal(obsC_value, 122)
  expect_equal(execCount(obsC), 2)
  expect_equal(obsD_value, 142)
  expect_equal(execCount(obsD), 3)

  # Changing A should invalidate obsC and obsD, and they should see updated
  # values for valueA, valueB, and funcB
  values$A <- 3
  flushReact()
  expect_equal(obsC_value, 143)
  expect_equal(execCount(obsC), 3)
  expect_equal(obsD_value, 143)
  expect_equal(execCount(obsD), 4)
})

test_that("Circular refs/reentrancy in reactive functions work", {

  values <- reactiveValues(A=3)

  funcB <- reactive(function() {
    # Each time fB executes, it reads and then writes valueA,
    # effectively invalidating itself--until valueA becomes 0.
    if (values$A == 0)
      return()
    values$A <- values$A - 1
    return(values$A)
  })

  obsC <- observe(function() {
    funcB()
  })

  flushReact()
  expect_equal(execCount(obsC), 4)

  values$A <- 3

  flushReact()
  expect_equal(execCount(obsC), 8)

})

test_that("Simple recursion", {

  values <- reactiveValues(A=5)
  funcB <- reactive(function() {
    if (values$A == 0)
      return(0)
    values$A <- values$A - 1
    funcB()
  })

  obsC <- observe(function() {
    funcB()
  })

  flushReact()
  expect_equal(execCount(obsC), 2)
  expect_equal(execCount(funcB), 6)
})

test_that("Non-reactive recursion", {
  nonreactiveA <- 3
  outputD <- NULL

  funcB <- reactive(function() {
    if (nonreactiveA == 0)
      return(0)
    nonreactiveA <<- nonreactiveA - 1
    return(funcB())
  })
  obsC <- observe(function() {
    outputD <<- funcB()
  })

  flushReact()
  expect_equal(execCount(funcB), 4)
  expect_equal(outputD, 0)
})

test_that("Circular dep with observer only", {

  values <- reactiveValues(A=3)
  obsB <- observe(function() {
    if (values$A == 0)
      return()
    values$A <- values$A - 1
  })

  flushReact()
  expect_equal(execCount(obsB), 4)
})

test_that("Writing then reading value is not circular", {

  values <- reactiveValues(3)
  funcB <- reactive(function() {
    values$A <- isolate(values$A) - 1
    values$A
  })

  obsC <- observe(function() {
    funcB()
  })

  flushReact()
  expect_equal(execCount(obsC), 1)

  values$A <- 10

  flushReact()
  expect_equal(execCount(obsC), 2)
})