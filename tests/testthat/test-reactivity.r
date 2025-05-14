test_that("reactive and reactiveVal are functions", {
  expect_s3_class(reactive({1}), "function")
  expect_s3_class(reactiveVal(1), "function")
})



test_that("ReactiveVal", {
  val <- reactiveVal()

  isolate({
    expect_null(val())

    # Set to a simple value
    val(1)
    expect_equal(val(), 1)

    # Set to a complex value
    val(cars)
    expect_equal(val(), cars)

    # Check that passing in an initial value works
    expect_equal(reactiveVal(10)(), 10)
  })

  o <- observe({
    val()
  })
  flushReact()
  expect_equal(execCount(o), 1)
  # Just making sure o is stable
  flushReact()
  expect_equal(execCount(o), 1)

  # Changing value causes o to invalidate
  val(10)
  flushReact()
  expect_equal(execCount(o), 2)

  # Setting new value that's same as current value is a no-op
  val(10)
  flushReact()
  expect_equal(execCount(o), 2)  #

  o$destroy()
})

test_that("ReactiveVals have independent dependencies", {
  # Issue 1710
  x <- reactiveVal(0)
  y <- reactiveVal(0)

  o <- observe({
    y()
  })

  # The observer always fires the first time
  x(1)
  flushReact()
  expect_equal(execCount(o), 1)

  # Changing x again shouldn't invalidate the observer
  x(2)
  flushReact()
  expect_equal(execCount(o), 1)

  o$destroy()
})


test_that("ReactiveVal labels", {
  val <- reactiveVal()
  expect_equal(attr(val, "label", exact = TRUE), "val")

  name.with.dots = reactiveVal()
  expect_equal(attr(name.with.dots, "label", exact = TRUE), "name.with.dots")
})

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
  expect_setequal(isolate(names(values)), c("a", "b"))
  expect_null(isolate(values$a))

  # Assigning NULL should keep object (not delete it), and set value to NULL
  values$b <- NULL
  expect_setequal(isolate(names(values)), c("a", "b"))
  expect_null(isolate(values$b))


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

test_that("reactiveValues keys are sorted", {
  values <- reactiveValues(b=2, a=0)
  values$C <- 13
  values$A <- 0
  values$c <- 3
  values$B <- 12
  # Setting an existing value shouldn't change order
  values$a <- 1
  values$A <- 11

  expect_identical(isolate(names(values)), c("b", "a", "C", "A", "c", "B"))
  expect_identical(
    isolate(reactiveValuesToList(values)),
    list(b=2, a=1, C=13, A=11, c=3, B=12)
  )
})

test_that("reactiveValues() has useful print method", {
  verify_output(test_path("print-reactiveValues.txt"), {
    x <- reactiveValues(x = 1, y = 2, z = 3)
    x
  })
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
  expect_setequal(isolate(names(values)), c(".B", "A"))
  # Assigning names fails
  expect_error(isolate(names(v) <- c('x', 'y')))

  expect_mapequal(isolate(reactiveValuesToList(values)), list(A=1))
  expect_mapequal(isolate(reactiveValuesToList(values, all.names=TRUE)), list(A=1, .B=2))


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


test_that("reactive() accepts injected quosures", {
  # Normal usage - no quosures
  a <- 1
  f <- reactive({ a + 10 })
  a <- 2
  expect_identical(isolate(f()), 12)

  # quosures can be used in reactive()
  a <- 1
  f <- reactive({ rlang::eval_tidy(rlang::quo(!!a + 10)) })
  a <- 2
  expect_identical(isolate(f()), 12)

  # inject() with quosures
  a <- 1
  exp <- rlang::quo(a + 10)
  f <- inject(reactive(!!exp))
  a <- 2
  expect_identical(isolate(f()), 12)

  # inject() with !!!
  a <- 1
  exp <- list(rlang::quo(a + 10))
  f <- inject(reactive(!!!exp))
  a <- 2
  expect_identical(isolate(f()), 12)

  # inject() with captured environment
  a <- 1
  exp <- local({
    q <- rlang::quo(a + 10)
    a <- 2
    q
  })
  f <- inject(reactive(!! exp ))
  a <- 3
  expect_identical(isolate(f()), 12)

  # inject() with nested quosures
  a <- 1
  y <- quo(a)
  exp <- quo(!!y + 10)
  a <- 2
  ff <- inject(reactive(!! exp ))
  a <- 3
  expect_identical(isolate(ff()), 13)
})

test_that("observe() accepts injected quosures", {
  # Normal usage - no quosures
  val <- NULL
  a <- 1
  observe({ val <<- a + 10 })
  a <- 2
  flushReact()
  expect_identical(val, 12)

  # quosures can be used in reactive()
  val <- NULL
  a <- 1
  f <- observe({ val <<- rlang::eval_tidy(rlang::quo(!!a + 10)) })
  a <- 2
  flushReact()
  expect_identical(val, 12)

  # inject() with quosures
  val <- NULL
  a <- 1
  exp <- rlang::quo(val <<- a + 10)
  f <- inject(observe(!!exp))
  a <- 2
  flushReact()
  expect_identical(val, 12)

  # inject() with !!!
  val <- NULL
  a <- 1
  exp <- list(quo(val <<- a + 10))
  f <- inject(observe(!!!exp))
  a <- 2
  flushReact()
  expect_identical(val, 12)

  # inject() with captured environment
  val <- NULL
  a <- 1
  exp <- local({
    q <- rlang::quo(val <<- a + 10)
    a <- 2
    q
  })
  f <- inject(observe(!! exp ))
  a <- 3
  flushReact()
  expect_identical(val, 12)

  # inject() with nested quosures
  val <- NULL
  a <- 1
  y <- quo(a)
  exp <- rlang::quo(val <<- !!y + 10)
  a <- 2
  f <- inject(observe(!!exp))
  a <- 3
  flushReact()
  expect_identical(val, 13)
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

  # Functions being passed to reactives is no longer treated specially
  fun <- reactive(function() { vals$A + 1 })
  expect_true(is.function(isolate(fun())))


  # Check that environment is correct - parent of parent environment should be
  # this one. Note that rlang::as_function() injects an intermediate
  # environment.
  this_env <- environment()
  fun <- reactive(environment())
  expect_identical(isolate(parent.env(parent.env(fun()))), this_env)

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

  # Functions are no longer treated specially
  observe(function() { valB <<- vals$A + 5 })
  flushReact()
  expect_equal(valB, 4)


  # Check that environment is correct - parent of parent environment should be
  # this one. rlang::as_function() injects one intermediate env.
  this_env <- environment()
  inside_env <- NULL
  fun <- observe(inside_env <<- environment())
  flushReact()
  expect_identical(parent.env(parent.env(inside_env)), this_env)
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


# The specific order that observers fire in does not necessarily need to be the
# one below. However, it is important that they fire in an order that is
# consistent across platforms, so that developers don't see one behavior on
# their dev platform and another on their deployment platform. (#2466)
test_that("Observers fire in consistent order across platforms", {

  # Reset the counter for the reactive environment. Not a good thing to do in
  # general, but necessary for this test.
  reactive_env <- .getReactiveEnvironment()
  reactive_env$.nextId <- 0L

  v <- reactiveVal(0)
  order <- list()
  order[1:20] <- list(integer())

  observe({
    order[[v()]] <<- c(order[[v()]], 1L)
    message(v(), ": observer 1")
  })
  observe({
    order[[v()]] <<- c(order[[v()]], 2L)
    message(v(), ": observer 2")
  })
  observe({
    order[[v()]] <<- c(order[[v()]], 3L)
    message(v(), ": observer 3")
  })

  for (i in 1:20) {
    suppressMessages({
      v(isolate(v()) + 1); shiny:::flushReact()
    })
  }

  expected_order <- list()
  expected_order[1:2] <- list(c(1L, 2L, 3L))
  expected_order[3:20] <- list(c(2L, 3L, 1L))
  expect_identical(order, expected_order)
})


test_that("installExprFunction doesn't rely on name being `expr`", {
  justExecute <- function(anExpression, envirToUse = parent.frame(), isQuoted = FALSE) {
    shiny:::installExprFunction(anExpression, "myFunc", envirToUse, quoted = isQuoted)
    myFunc()
  }

  expect_identical(-1, justExecute({-1}))
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
  expect_no_error(reactive({}))
  expect_no_error(reactive(NULL))
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

test_that("observers are garbage collected when destroyed", {
  domain <- createMockDomain()
  rv <- reactiveValues(x = 1)

  # Auto-destroy. GC on domain end.
  a <- observe(rv$x, domain = domain)
  # No auto-destroy. GC with rv.
  b <- observe(rv$x, domain = domain, autoDestroy = FALSE)
  # No auto-destroy and no reactive dependencies. GC immediately.
  c <- observe({}, domain = domain)
  c$setAutoDestroy(FALSE)
  # Similar to b, but we'll set it to autoDestroy later.
  d <- observe(rv$x, domain = domain, autoDestroy = FALSE)
  # Like a, but we'll destroy it immediately.
  e <- observe(rx$x, domain = domain)
  e$destroy()

  collected <- new.env(parent = emptyenv())

  reg.finalizer(a, function(o) collected$a <- TRUE)
  reg.finalizer(b, function(o) collected$b <- TRUE)
  reg.finalizer(c, function(o) collected$c <- TRUE)
  reg.finalizer(d, function(o) collected$d <- TRUE)
  reg.finalizer(e, function(o) collected$e <- TRUE)

  rm(list = c("a", "b", "c", "e")) # Not "d"

  gc()
  # Nothing can be GC'd yet, because all of the observers are
  # pending execution (i.e. waiting for flushReact).
  expect_equal(ls(collected), character())

  flushReact()
  # Now "c" can be garbage collected, because it ran and took
  # no dependencies (and isn't tied to the session in any way).
  # And "e" can also be garbage collected, it's been destroyed.
  gc()
  expect_equal(ls(collected), c("c", "e"))

  domain$end()
  # We can GC "a" as well; even though it references rv, it is
  # destroyed when the session ends.
  gc()
  expect_equal(sort(ls(collected)), c("a", "c", "e"))

  # It's OK to turn on auto-destroy even after the session was
  # destroyed.
  d$setAutoDestroy(TRUE)
  # This should no-op.
  d$setAutoDestroy(FALSE)
  rm(d)
  gc()
  expect_equal(sort(ls(collected)), c("a", "c", "d", "e"))

  rm(rv)
  # Both rv and "b" can now be collected.
  gc()
  expect_equal(sort(ls(collected)), c("a", "b", "c", "d", "e"))
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
  suppress_stacktrace(
    # Errors in reactive are translated to warnings in observers by default
    expect_warning(expect_warning(flushReact()))
  )
  # Both observers should run up until the reactive that errors
  expect_true(all(c(n11, n12, n21, n22) == c(2,1,2,1)))

  # Nothing should happen on next flush
  flushReact()
  expect_true(all(c(n11, n12, n21, n22) == c(2,1,2,1)))
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


for (do_priming in c(TRUE, FALSE)) {
  label <- if (do_priming) "with priming" else "without priming"
  test_that(sprintf("debounce/throttle work properly (%s)", label), {
    # Some of the CRAN test machines are heavily loaded and so the timing for
    # these tests isn't reliable. https://github.com/rstudio/shiny/pull/2789
    skip_on_cran()

    # The changing of rv$a will be the (chatty) source of reactivity.
    rv <- reactiveValues(a = 0)

    # This observer will be what changes rv$a.
    src <- observe({
      invalidateLater(300)
      rv$a <- isolate(rv$a) + 1
    })
    on.exit(src$destroy(), add = TRUE)

    # Make a debounced reactive to test.
    dr <- debounce(reactive(rv$a), 500)

    # Make a throttled reactive to test.
    tr <- throttle(reactive(rv$a), 500)

    # Keep track of how often dr/tr are fired
    dr_fired <- 0
    dr_monitor <- observeEvent(dr(), {
      dr_fired <<- dr_fired + 1
    })
    on.exit(dr_monitor$destroy(), add = TRUE)

    tr_fired <- 0
    tr_monitor <- observeEvent(tr(), {
      tr_fired <<- tr_fired + 1
    })
    on.exit(tr_monitor$destroy(), add = TRUE)

    # Starting values are both 0. Earlier I found that the tests behaved
    # differently if I accessed the values of dr/tr before the first call to
    # flushReact(). That bug was fixed, but to ensure that similar bugs don't
    # appear undetected, we run this test with and without do_priming.
    if (do_priming) {
      expect_identical(isolate(dr()), 0)
      expect_identical(isolate(tr()), 0)
    }

    # Pump timer and reactives for about 1.3 seconds
    stopAt <- Sys.time() + 1.3
    while (Sys.time() < stopAt) {
      timerCallbacks$executeElapsed()
      flushReact()
      Sys.sleep(0.001)
    }

    # dr() should not have had time to fire, other than the initial run, since
    # there haven't been long enough gaps between invalidations.
    expect_identical(dr_fired, 1)
    # The value of dr() should not have updated either.
    expect_identical(isolate(dr()), 0)

    # tr() however, has had time to fire multiple times and update its value.
    expect_identical(tr_fired, 3)
    expect_identical(isolate(tr()), 4)

    # Now let some time pass without any more updates.
    src$destroy() # No more updates
    stopAt <- Sys.time() + 1
    while (Sys.time() < stopAt) {
      timerCallbacks$executeElapsed()
      flushReact()
      Sys.sleep(0.001)
    }

    # dr should've fired, and we should have converged on the right answer.
    expect_identical(dr_fired, 2)
    isolate(expect_identical(rv$a, dr()))
    # be sure tr() converged on the right answer; (We've already confirmed throttle-like behavior above)
    isolate(expect_identical(rv$a, tr()))
  })
}

test_that("reactive domain works across async handlers", {
  obj <- new.env()
  hasReactiveDomain <- NULL
  withReactiveDomain(obj, {
    promises::then(
      promises::promise_resolve(TRUE),
      ~{hasReactiveDomain <<- identical(getDefaultReactiveDomain(), obj)}
    )
  })

  while (is.null(hasReactiveDomain) && !later::loop_empty()) {
    later::run_now()
  }

  testthat::expect_true(hasReactiveDomain)
})

# For #2441, #2423
test_that("Unreachable reactives are GC'd", {
  v <- reactiveVal(1)
  r <- reactive({
    v()
    12345
  })
  o <- observe({
    r()
  })
  # Finalizer on the reactive's underlying Observable object
  r_finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) {
    r_finalized <<- TRUE
  })

  # Finalizer on the Observer
  o_finalized <- FALSE
  reg.finalizer(o, function(e) {
    o_finalized <<- TRUE
  })

  flushReact()
  gc()
  expect_false(r_finalized)

  rm(r) # Remove the only (strong) reference to r
  gc()
  expect_true(r_finalized)
  expect_false(o_finalized)

  rm(o) # Remove the only reference to o
  gc()
  expect_true(o_finalized)

  rm(v)
  gc()

  # Same, with reactiveValues instead of reactiveVal
  v <- reactiveValues(x = 1)
  r <- reactive({
    v$x
    12345
  })
  o <- observe({
    r()
  })
  # Finalizer on the reactive's underlying Observable object
  r_finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) {
    r_finalized <<- TRUE
  })

  # Finalizer on the Observer
  o_finalized <- FALSE
  reg.finalizer(o, function(e) {
    o_finalized <<- TRUE
  })

  flushReact()
  gc()
  expect_false(r_finalized)

  rm(r) # Remove the only (strong) reference to r
  gc()
  expect_true(r_finalized)
  expect_false(o_finalized)

  rm(o) # Remove the only reference to o
  gc()
  expect_true(o_finalized)
})



test_that("Reactive contexts are not GC'd too early", {
  # When a ReactiveVal or ReactiveValue has an dependency arrow pointing to a
  # reactive expression (Observable object), it's implemented by having a weak
  # reference to a reactive context. We need to make sure that the reactive
  # context is not GC'd too early. This is done by having the Observable have a
  # strong reference to the context.

  # Check reactiveVal
  v <- reactiveVal(1)
  r <- reactive({
    v()
  })
  o <- observe({
    r()
    gc()
  })
  # Finalizer on the reactive's underlying Observable object
  r_finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) {
    r_finalized <<- TRUE
  })

  for (i in 1:3) {
    v(isolate(v()) + 1)
    flushReact()
  }

  expect_identical(execCount(r), 3L)
  expect_false(r_finalized)
  o$destroy()
  rm(v, r, o)
  gc()
  expect_true(r_finalized)


  # Same, but with reactiveValues
  v <- reactiveValues(x=1)
  r <- reactive({
    v$x
  })
  o <- observe({
    r()
    gc()
  })
  # Finalizer on the reactive's underlying Observable object
  r_finalized <- FALSE
  reg.finalizer(attr(r, "observable"), function(e) {
    r_finalized <<- TRUE
  })

  for (i in 1:3) {
    v$x <- (isolate(v$x) + 1)
    flushReact()
  }

  expect_identical(execCount(r), 3L)
  expect_false(r_finalized)
})


test_that("reactivePoll doesn't leak observer (#1548)", {
  i <- 0
  count <- reactivePoll(50, NULL,
    checkFunc = function() {
      i <<- i + 1
      i
    },
    valueFunc = function() i
  )

  observe({
    count()
  })

  while (i < 3) {
    Sys.sleep(0.05)
    shiny:::timerCallbacks$executeElapsed()
    shiny:::flushReact()
  }

  # Removing the reference to count means that no one can use it anymore, and so
  # the finalizer should run. The finalizer sets a flag which will allow the
  # observer (which calls `checkFunc`) to run one more time; in that run, it
  # will remove itself.
  rm(count)
  gc()

  # If the reactivePoll was cleaned up, then the first run of this loop will
  # increment i (bringing its value to 4), but in that run, the observer will
  # remove itself so subsequent runs will no longer run `checkFunc`.
  for (n in 1:3) {
    Sys.sleep(0.05)
    shiny:::timerCallbacks$executeElapsed()
    shiny:::flushReact()
  }

  expect_equal(i, 3L)
})

test_that("reactivePoll prefers session$scheduleTask", {
  called <- 0
  session <- list(reactlog = function(...){}, onEnded = function(...){}, .scheduleTask = function(millis, cb){
    expect_equal(millis, 50)
    called <<- called + 1
  })

  count <- reactivePoll(50, session, function(){}, function(){})
  observe({
    count()
  })

  for (i in 1:4) {
    Sys.sleep(0.05)
    shiny:::flushReact()
  }
  expect_gt(called, 0)
})

test_that("invalidateLater prefers session$scheduleTask", {
  called <- 0
  session <- list(reactlog = function(...){}, onEnded = function(...){}, .scheduleTask = function(millis, cb){
    expect_equal(millis, 10)
    called <<- called + 1
  })

  observe({
    invalidateLater(10, session)
  })

  for (i in 1:4) {
    Sys.sleep(0.05)
    shiny:::flushReact()
  }
  expect_gt(called, 0)
})

test_that("reactiveTimer prefers session$scheduleTask", {
  called <- 0
  session <- list(reactlog = function(...){}, onEnded = function(...){}, .scheduleTask = function(millis, cb){
    expect_equal(millis, 10)
    called <<- called + 1
  })

  rt <- reactiveTimer(10, session)
  observe({
    rt()
  })

  for (i in 1:4) {
    Sys.sleep(0.05)
    shiny:::flushReact()
  }
  expect_gt(called, 0)
})


test_that("Reactive expression visibility", {
  res <- NULL
  rv <- reactive(1)
  o <- observe({
    res <<- withVisible(rv())
  })
  flushReact()
  expect_identical(res, list(value = 1, visible = TRUE))


  res <- NULL
  rv <- reactive(invisible(1))
  o <- observe({
    res <<- withVisible(rv())
  })
  flushReact()
  expect_identical(res, list(value = 1, visible = FALSE))

  # isolate
  expect_identical(
    withVisible(isolate(1)),
    list(value = 1, visible = TRUE)
  )
  expect_identical(
    withVisible(isolate(invisible(1))),
    list(value = 1, visible = FALSE)
  )
})


test_that("Reactive expression labels", {
  r <- list()

  # Automatic label
  r$x <- reactive({
    a+1;b+  2
  })
  # Printed output - uses expression, not `label`
  expect_identical(
    capture.output(print(r$x)),
    c("reactive({", "    a + 1", "    b + 2", "}) ")
  )
  # Label used for debugging
  expect_identical(
    as.character(attr(r$x, "observable")$.label),
    "r$x"
  )

  # With explicit label
  r$y <- reactive({ a+1;b+  2 }, label = "hello")
  expect_identical(
    capture.output(print(r$y)),
    c("reactive({", "    a + 1", "    b + 2", "}) ")
  )
  expect_identical(
    as.character(attr(r$y, "observable")$.label),
    "hello"
  )
})


test_that("Contexts can be masked off", {
  expect_error(
    {
      r <- reactiveVal()
      isolate({
        maskReactiveContext({
          r()
        })
      })
    },
    regexp = "Operation not allowed without an active reactive context"
  )
})


test_that("Contexts can be masked off via promise domains", {
  r <- reactiveVal()
  done <- FALSE
  isolate({
    maskReactiveContext({
      promises::promise_resolve(NULL)$then(function(value) {
        done <<- TRUE
        expect_error(
          {
            r()
          },
          regexp = "Operation not allowed without an active reactive context"
        )
      })
    })
  })
  while (!done) {
    later::run_now(all=FALSE)
  }
})

