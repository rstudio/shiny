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
  expect_error(reactiveValues(1))
  expect_error(reactiveValues(1, b=2))

  # Error: assignment to readonly values
  values <- .createReactiveValues(ReactiveValues$new(), readonly = TRUE)
  expect_error(values$a <- 1)
})


# Test for overreactivity. funcB has an indirect dependency on valueA (via
# funcA) and also a direct dependency on valueA. When valueA changes, funcB
# should only execute once.
test_that("Functions are not over-reactive", {

  values <- reactiveValues(A=10)

  funcA <- reactive({
    values$A
  })

  funcB <- reactive({
    funcA()
    values$A
  })

  obsC <- observe({
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
  funcB  <- reactive({
    values$A + 5
  })
  obsC <- observe({
    observed_value1 <<-  funcB() * values$A
  })
  obsD <- observe({
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

  obsB <- observe({
    values$C <- values$A > 0
  })

  funcD <- reactive({
    values$C
  })

  obsE <- observe({
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

  funcA <- reactive({
    values$A > 0
  })

  funcB <- reactive({
    funcA()
  })

  obsC <- observe({
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
  funcB  <- reactive({
    values$A + 5
  })
  obsC <- observe({
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
  funcB <- reactive({
    values$A + 5
  })
  obsC <- observe({
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
  funcB <- reactive({
    values$B + 100
  })

  # References to valueB and funcB are isolated
  obsC <- observe({
    obsC_value <<-
      values$A + isolate(values$B) + isolate(funcB())
  })

  # In contrast with obsC, this has a non-isolated reference to funcB
  obsD <- observe({
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


test_that("isolate() evaluates expressions in calling environment", {
  outside <- 1
  inside <- 1
  loc <- 1

  outside <- isolate(2)      # Assignment outside isolate
  isolate(inside <- 2)       # Assignment inside isolate
  # Should affect vars in the calling environment
  expect_equal(outside, 2)
  expect_equal(inside, 2)

  isolate(local(loc <<- 2))  # <<- inside isolate(local)
  isolate(local(loc <- 3))   # <- inside isolate(local) - should have no effect
  expect_equal(loc, 2)
})


test_that("Circular refs/reentrancy in reactive functions work", {

  values <- reactiveValues(A=3)

  funcB <- reactive({
    # Each time fB executes, it reads and then writes valueA,
    # effectively invalidating itself--until valueA becomes 0.
    if (values$A == 0)
      return()
    values$A <- values$A - 1
    return(values$A)
  })

  obsC <- observe({
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
  funcB <- reactive({
    if (values$A == 0)
      return(0)
    values$A <- values$A - 1
    funcB()
  })

  obsC <- observe({
    funcB()
  })

  flushReact()
  expect_equal(execCount(obsC), 2)
  expect_equal(execCount(funcB), 6)
})

test_that("Non-reactive recursion", {
  nonreactiveA <- 3
  outputD <- NULL

  funcB <- reactive({
    if (nonreactiveA == 0)
      return(0)
    nonreactiveA <<- nonreactiveA - 1
    return(funcB())
  })
  obsC <- observe({
    outputD <<- funcB()
  })

  flushReact()
  expect_equal(execCount(funcB), 4)
  expect_equal(outputD, 0)
})

test_that("Circular dep with observer only", {

  values <- reactiveValues(A=3)
  obsB <- observe({
    if (values$A == 0)
      return()
    values$A <- values$A - 1
  })

  flushReact()
  expect_equal(execCount(obsB), 4)
})

test_that("Writing then reading value is not circular", {

  values <- reactiveValues(A=3)
  funcB <- reactive({
    values$A <- isolate(values$A) - 1
    values$A
  })

  obsC <- observe({
    funcB()
  })

  flushReact()
  expect_equal(execCount(obsC), 1)

  values$A <- 10

  flushReact()
  expect_equal(execCount(obsC), 2)
})

test_that("names() and reactiveValuesToList()", {

  values <- reactiveValues(A=1, .B=2)

  # Dependent on names
  depNames <- observe({
    names(values)
  })

  # Dependent on all non-hidden objects
  depValues <- observe({
    reactiveValuesToList(values)
  })

  # Dependent on all objects, including hidden
  depAllValues <- observe({
    reactiveValuesToList(values, all.names = TRUE)
  })

  # names() returns all names
  expect_equal(sort(isolate(names(values))), sort(c(".B", "A")))
  # Assigning names fails
  expect_error(isolate(names(v) <- c('x', 'y')))

  expect_equal(isolate(reactiveValuesToList(values)), list(A=1))
  expect_equal(isolate(reactiveValuesToList(values, all.names=TRUE)), list(A=1, .B=2))


  flushReact()
  expect_equal(execCount(depNames), 1)
  expect_equal(execCount(depValues), 1)
  expect_equal(execCount(depAllValues), 1)

  # Update existing variable
  values$A <- 2
  flushReact()
  expect_equal(execCount(depNames), 1)
  expect_equal(execCount(depValues), 2)
  expect_equal(execCount(depAllValues), 2)

  # Update existing hidden variable
  values$.B <- 3
  flushReact()
  expect_equal(execCount(depNames), 1)
  expect_equal(execCount(depValues), 2)
  expect_equal(execCount(depAllValues), 3)

  # Add new variable
  values$C <- 1
  flushReact()
  expect_equal(execCount(depNames), 2)
  expect_equal(execCount(depValues), 3)
  expect_equal(execCount(depAllValues), 4)

  # Add new hidden variable
  values$.D <- 1
  flushReact()
  expect_equal(execCount(depNames), 3)
  expect_equal(execCount(depValues), 3)
  expect_equal(execCount(depAllValues), 5)
})

test_that("Observer pausing works", {
  values <- reactiveValues(a=1)

  funcA <- reactive({
    values$a
  })

  obsB <- observe({
    funcA()
  })

  # Important: suspend() only affects observer at invalidation time

  # Observers are invalidated at creation time, so it will run once regardless
  # of being suspended
  obsB$suspend()
  flushReact()
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(obsB), 1)

  # When resuming, if nothing changed, don't do anything
  obsB$resume()
  flushReact()
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(obsB), 1)

  # Make sure suspended observers do not flush, but do invalidate
  obsB_invalidated <- FALSE
  obsB$onInvalidate(function() {obsB_invalidated <<- TRUE})
  obsB$suspend()
  values$a <- 2
  flushReact()
  expect_equal(obsB_invalidated, TRUE)
  expect_equal(execCount(funcA), 1)
  expect_equal(execCount(obsB), 1)

  obsB$resume()
  values$a <- 2.5
  obsB$suspend()
  flushReact()
  expect_equal(execCount(funcA), 2)
  expect_equal(execCount(obsB), 2)

  values$a <- 3
  flushReact()

  expect_equal(execCount(funcA), 2)
  expect_equal(execCount(obsB), 2)

  # If onInvalidate() is added _after_ obsB is suspended and the values$a
  # changes, then it shouldn't get run (onInvalidate runs on invalidation, not
  # on flush)
  values$a <- 4
  obsB_invalidated2 <- FALSE
  obsB$onInvalidate(function() {obsB_invalidated2 <<- TRUE})
  obsB$resume()
  flushReact()

  expect_equal(execCount(funcA), 3)
  expect_equal(execCount(obsB), 3)
  expect_equal(obsB_invalidated2, FALSE)
})

test_that("suspended/resumed observers run at most once", {

  values <- reactiveValues(A=1)
  obs <- observe({
    values$A
  })
  expect_equal(execCount(obs), 0)

  # First flush should run obs once
  flushReact()
  expect_equal(execCount(obs), 1)

  # Modify the dependency at each stage of suspend/resume/flush should still
  # only result in one run of obs()
  values$A <- 2
  obs$suspend()
  values$A <- 3
  obs$resume()
  values$A <- 4
  flushReact()
  expect_equal(execCount(obs), 2)

})


test_that("reactive() accepts quoted and unquoted expressions", {
  vals <- reactiveValues(A=1)

  # Unquoted expression, with curly braces
  fun <- reactive({ vals$A + 1 })
  expect_equal(isolate(fun()), 2)

  # Unquoted expression, no curly braces
  fun <- reactive(vals$A + 1)
  expect_equal(isolate(fun()), 2)

  # Quoted expression
  fun <- reactive(quote(vals$A + 1), quoted = TRUE)
  expect_equal(isolate(fun()), 2)

  # Quoted expression, saved in a variable
  q_expr <- quote(vals$A + 1)
  fun <- reactive(q_expr, quoted = TRUE)
  expect_equal(isolate(fun()), 2)

  # If function is used, work, but print message
  expect_message(fun <- reactive(function() { vals$A + 1 }))
  expect_equal(isolate(fun()), 2)


  # Check that environment is correct - parent environment should be this one
  this_env <- environment()
  fun <- reactive(environment())
  expect_identical(isolate(parent.env(fun())), this_env)

  # Sanity check: environment structure for a reactive() should be the same as for
  # a normal function
  fun <- function() environment()
  expect_identical(parent.env(fun()), this_env)
})

test_that("observe() accepts quoted and unquoted expressions", {
  vals <- reactiveValues(A=0)
  valB <- 0

  # Unquoted expression, with curly braces
  observe({ valB <<- vals$A + 1})
  flushReact()
  expect_equal(valB, 1)

  # Unquoted expression, no curly braces
  observe({ valB <<- vals$A + 2})
  flushReact()
  expect_equal(valB, 2)

  # Quoted expression
  observe(quote(valB <<- vals$A + 3), quoted = TRUE)
  flushReact()
  expect_equal(valB, 3)

  # Quoted expression, saved in a variable
  q_expr <- quote(valB <<- vals$A + 4)
  fun <- observe(q_expr, quoted = TRUE)
  flushReact()
  expect_equal(valB, 4)

  # If function is used, work, but print message
  expect_message(observe(function() { valB <<- vals$A + 5 }))
  flushReact()
  expect_equal(valB, 5)


  # Check that environment is correct - parent environment should be this one
  this_env <- environment()
  inside_env <- NULL
  fun <- observe(inside_env <<- environment())
  flushReact()
  expect_identical(parent.env(inside_env), this_env)
})

test_that("Observer priorities are respected", {
  results <- c()
  observe(results <<- c(results, 10), priority=10)
  observe(results <<- c(results, 30), priority=30)
  observe(results <<- c(results, 20), priority=20L)
  observe(results <<- c(results, 21), priority=20)
  observe(results <<- c(results, 22), priority=20L)

  flushReact()

  expect_identical(results, c(30, 20, 21, 22, 10))
})

test_that("reactivePoll and reactiveFileReader", {
  path <- tempfile('file')
  on.exit(unlink(path))
  write.csv(cars, file=path, row.names=FALSE)
  rfr <- reactiveFileReader(100, NULL, path, read.csv)
  expect_equal(isolate(rfr()), cars)

  write.csv(rbind(cars, cars), file=path, row.names=FALSE)
  Sys.sleep(0.15)
  timerCallbacks$executeElapsed()
  expect_equal(isolate(rfr()), cars)
  flushReact()
  expect_equal(isolate(rfr()), rbind(cars, cars))
})


test_that("classes of reactive object", {
  v <- reactiveValues(a = 1)
  r <- reactive({ v$a + 1 })
  o <- observe({ print(r()) })

  expect_false(is.reactivevalues(12))
  expect_true(is.reactivevalues(v))
  expect_false(is.reactivevalues(r))
  expect_false(is.reactivevalues(o))

  expect_false(is.reactive(12))
  expect_false(is.reactive(v))
  expect_true(is.reactive(r))
  expect_false(is.reactive(o))

  o$destroy()
})

test_that("{} and NULL also work in reactive()", {
  reactive({})
  reactive(NULL)
})

test_that("shiny.suppressMissingContextError option works", {
  options(shiny.suppressMissingContextError=TRUE)
  on.exit(options(shiny.suppressMissingContextError=FALSE), add = TRUE)

  expect_true(reactive(TRUE)())
})

test_that("reactive domains are inherited", {

  domainA <- createMockDomain()
  domainB <- createMockDomain()

  local({
    domainY <- NULL
    domainZ <- NULL
    x <- observe({

      y <- observe({
        # Should be domainA (inherited from observer x)
        domainY <<- getDefaultReactiveDomain()
      })

      z <- observe({
        # Should be domainB (explicitly passed in)
        domainZ <<- getDefaultReactiveDomain()
      }, domain = domainB)

    }, domain = domainA)

    flushReact()
    flushReact()

    expect_identical(domainY, domainA)
    expect_identical(domainZ, domainB)
  })

  local({
    domainY <- 1
    x <- NULL
    y <- NULL
    z <- NULL
    r3 <- NULL
    domainR3 <- NULL

    r1 <- reactive({
      y <<- observe({
        # Should be NULL (r1 has no domain)
        domainY <<- getDefaultReactiveDomain()
      })
    })
    r2 <- reactive({
      z <<- observe({
        # Should be domainB (r2 has explicit domainB)
        domainZ <<- getDefaultReactiveDomain()
      })
    }, domain = domainB)

    observe({
      r3 <<- reactive({
        # This should be domainA. Doesn't matter where r3 is invoked, it only
        # matters where it was created.
        domainR3 <<- getDefaultReactiveDomain()
      })
      r1()
      r2()
    }, domain = domainA)

    flushReact()
    flushReact()
    isolate(r3())

    expect_identical(execCount(y), 1L)
    expect_identical(execCount(z), 1L)
    expect_identical(domainY, NULL)
    expect_identical(domainZ, domainB)
    expect_identical(domainR3, domainA)
  })
})

test_that("observers autodestroy (or not)", {

  domainA <- createMockDomain()
  local({
    a <- observe(NULL, domain = domainA)

    b <- observe(NULL, domain = domainA, autoDestroy = FALSE)

    c <- observe(NULL, domain = domainA)
    c$setAutoDestroy(FALSE)

    d <- observe(NULL, domain = domainA, autoDestroy = FALSE)
    d$setAutoDestroy(TRUE)

    e <- observe(NULL)

    domainA$end()

    flushReact()

    expect_identical(execCount(a), 0L)
    expect_identical(execCount(b), 1L)
    expect_identical(execCount(c), 1L)
    expect_identical(execCount(d), 0L)
    expect_identical(execCount(e), 1L)
  })
})

test_that("maskReactiveContext blocks use of reactives", {
  vals <- reactiveValues(x = 123)

  # Block reactive contexts (created by isolate)
  expect_error(isolate(maskReactiveContext(vals$x)))
  expect_error(isolate(isolate(maskReactiveContext(vals$x))))

  # Reactive contexts within maskReactiveContext shouldn't be blocked
  expect_identical(maskReactiveContext(isolate(vals$x)), 123)
  expect_identical(isolate(maskReactiveContext(isolate(vals$x))), 123)
})

test_that("Flush completes even when errors occur", {
  vals <- reactiveValues(x = 1)

  r <- reactive({
    if (vals$x == 0) stop("x is zero!")
    else vals$x
  })

  # Set up counters
  n11 <- n12 <- n21 <- n22 <- 0

  observe({
    n11 <<- n11 + 1
    r()
    n12 <<- n12 + 1
  })
  observe({
    n21 <<- n21 + 1
    r()
    n22 <<- n22 + 1
  })

  flushReact()
  expect_true(all(c(n11, n12, n21, n22) == 1))

  # Trigger an error
  vals$x <- 0
  # Errors in reactive are translated to warnings in observers by default
  expect_warning(flushReact())
  # Both observers should run up until the reactive that errors
  expect_true(all(c(n11, n12, n21, n22) == c(2,1,2,1)))

  # Nothing should happen on next flush
  flushReact()
  expect_true(all(c(n11, n12, n21, n22) == c(2,1,2,1)))
})


test_that("Alternate error handler function", {
  vals <- reactiveValues(x = 0)

  r <- reactive({
    stop("error in reactive!")
  })

  observe({
    r()
  })
  observe({
    r()
  })

  # Alternate observer error handler - increment counter
  ec <- 0
  options(shiny.observer.error = function(e, label = NULL, domain = NULL) {
    ec <<- ec + 1
  })
  on.exit(options(shiny.observer.error = NULL))

  flushReact()
  expect_identical(ec, 2)
})

test_that("event handling helpers take correct dependencies", {
  vals <- reactiveValues(action = NULL, x = 1)

  o1_count <- 0
  o1 <- observeEvent(vals$action, {
    vals$x
    o1_count <<- o1_count + 1
  })
  o2_count <- 0
  o2 <- observeEvent(ignoreNULL = FALSE, vals$action, {
    vals$x
    o2_count <<- o2_count + 1
  })
  r1 <- eventReactive(vals$action, {
    vals$x
  })
  r2 <- eventReactive(ignoreNULL = FALSE, vals$action, {
    vals$x
  })

  flushReact()

  expect_error(isolate(r1()))
  expect_identical(isolate(r2()), 1)
  expect_equal(o1_count, 0)
  expect_equal(o2_count, 1)
  expect_equal(execCount(o1), 1)
  expect_equal(execCount(o2), 1)

  vals$x <- 2
  flushReact()

  expect_error(isolate(r1()))
  expect_identical(isolate(r2()), 1)
  expect_equal(o1_count, 0)
  expect_equal(o2_count, 1)
  expect_equal(execCount(o1), 1)
  expect_equal(execCount(o2), 1)

  vals$action <- 1
  flushReact()
  expect_identical(isolate(r1()), 2)
  expect_identical(isolate(r2()), 2)
  expect_equal(o1_count, 1)
  expect_equal(o2_count, 2)
  expect_equal(execCount(o1), 2)
  expect_equal(execCount(o2), 2)
})
