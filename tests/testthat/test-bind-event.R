
# Note that there are some tests for bindEvent() and caching in
# test-with-cache.R.

test_that("bindEvent and observers", {
  trigger <- reactiveVal(1)
  val <- reactiveVal(10)

  vals <- numeric()
  o <- bindEvent(
    trigger(),
    x = observe({
      vals <<- c(vals, val())
    })
  )

  flushReact()
  expect_identical(vals, 10)

  # Changing val has no effect
  val(20)
  flushReact()
  expect_identical(vals, 10)

  # Changing trigger causes the observer to execute
  trigger(2)
  flushReact()
  expect_identical(vals, c(10, 20))

  trigger(3)
  flushReact()
  expect_identical(vals, c(10, 20, 20))
})


test_that("bindEvent alters observers in place", {
  v <- reactiveVal(1)
  o <- observe({ v() })
  o1 <- bindEvent(o, v())

  # o and o1 are the same object
  expect_identical(o, o1)

  # Can't call bindEvent twice on an observer
  expect_error(bindEvent(o, v()))
})


test_that("ignoreNULL works", {
  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, ignoreNULL = FALSE)
  flushReact()
  expect_identical(n, 1)

  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, ignoreNULL = TRUE)
  flushReact()
  expect_identical(n, 0)

  # Two NULLs in the `...` get aggregated into a list, so the result is not
  # NULL.
  n <- 0
  observe({ n <<- n+1 }) %>% bindEvent(NULL, NULL, ignoreNULL = TRUE)
  flushReact()
  expect_identical(n, 1)
})


test_that("once=TRUE works", {
  n <- 0
  v <- reactiveVal(1)
  observe({ n <<- n + 1 }) %>% bindEvent(v(), once = FALSE)
  flushReact()
  expect_identical(n, 1)
  v(2)
  flushReact()
  expect_identical(n, 2)

  n <- 0
  v <- reactiveVal(1)
  observe({ n <<- n + v() }) %>% bindEvent(v(), once = TRUE)
  flushReact()
  expect_identical(n, 1)
  v(2)
  flushReact()
  expect_identical(n, 1)
})

test_that("bindEvent.Observer holds back every first run while the session answers a resume", {
  count <- function() list2env(list(plain = 0, closure = 0, ignore = 0))
  handlers <- function(s, runs) withMockContext(s, {
    observeEvent(s$input$x, runs$plain <- runs$plain + 1)
    observeEvent(s$input$x, { shared <<- 1; runs$closure <- runs$closure + 1 })
    observeEvent(s$input$x, runs$ignore <- runs$ignore + 1, ignoreInit = TRUE)
  })
  tally <- function(runs) c(runs$plain, runs$closure, runs$ignore)
  shared <- NULL

  runs <- count()
  s <- MockShinySession$new()
  handlers(s, runs)
  s$setInputs(x = 1)                                   # not resuming: all but ignoreInit ran
  expect_identical(tally(runs), c(1, 1, 0))

  # No registry at all (resume off): the field alone holds back.
  withr::local_envvar(SHINY_RESUME = "FALSE")
  runs2 <- count()
  s2 <- MockShinySession$new()
  expect_null(s2$.snapshotRegistry)
  handlers(s2, runs2)
  snapshot_begin_hold_back(s2)
  s2$setInputs(x = 1)                                  # the first flush: every first run held back
  expect_identical(tally(runs2), c(0, 0, 0))
  expect_false(s2$.resumeHoldBack)                     # cleared after the first flush
  s2$setInputs(x = 2)                                  # the dependency was registered
  expect_identical(tally(runs2), c(1, 1, 1))
})

test_that("a held-back handler that assigns closure variables is named in devmode", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  n <- 0
  last <- NULL
  s <- MockShinySession$new()
  one <- withMockContext(s, observeEvent(s$input$send, n <<- n + 1))
  two <- withMockContext(s, observeEvent(s$input$send, { n <<- n + 1; last <<- "x" }))
  withMockContext(s, observeEvent(s$input$send, NULL))
  snapshot_begin_hold_back(s)
  s$setInputs(send = 1)
  expect_identical(n, 0)
  expect_setequal(seen, c(
    sprintf("%s was held back on resume; it assigns n, whose value was not saved. Keep it in a reactiveVal() or session$userData, or rebuild it in session$onResume().", one$.label),
    sprintf("%s was held back on resume; it assigns n, last, whose values were not saved. Keep them in a reactiveVal() or session$userData, or rebuild them in session$onResume().", two$.label)
  ))
  s$setInputs(send = 2)                                # a run that is not held back says nothing
  expect_identical(n, 2)
  expect_length(seen, 2)
})

test_that("an observer created after the first flush of a resume runs normally", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  runs <- 0
  s <- MockShinySession$new()
  s$setInputs(x = 1)
  snapshot_begin_hold_back(s)
  s$flushReact()
  withMockContext(s, observeEvent(s$input$x, runs <<- runs + 1))
  s$flushReact()
  expect_identical(runs, 1)
})
