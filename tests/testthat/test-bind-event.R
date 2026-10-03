
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

test_that("bindEvent.Observer holds back the first run on a resumed session unless marked or hazardous", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  # Counters live in environments: a `runs["x"] <<- ...` handler would itself
  # assign a closure variable and so count as a hazard.
  count <- function() list2env(list(plain = 0, marked = 0, hazard = 0))
  handlers <- function(s, runs) withMockContext(s, {
    observeEvent(s$input$x, runs$plain <- runs$plain + 1)
    m <- observeEvent(s$input$x, runs$marked <- runs$marked + 1)
    disableResume(m)
    observeEvent(s$input$x, { shared <<- 1; runs$hazard <- runs$hazard + 1 })
  })
  tally <- function(runs) c(runs$plain, runs$marked, runs$hazard)
  shared <- NULL

  runs <- count()
  s <- MockShinySession$new()
  handlers(s, runs)
  s$setInputs(x = 1)                                   # not restoring: all three ran
  expect_identical(tally(runs), c(1, 1, 1))

  # Restoring mode is entered by resume(); the handlers are created inside it.
  runs2 <- count()
  s2 <- MockShinySession$new()
  rec <- list(format = 2L, complete = TRUE, blocked = list(), sources = list(), userData = list(),
              inputs = list(x = 1), clientData = list(), fileInputs = character(0))
  s2$.snapshotRegistry$resume(rec, list(inputs = list(x = 1), clientData = list(), fileInputs = character(0)), FALSE,
    function() handlers(s2, runs2))
  s2$flushReact()
  expect_identical(tally(runs2), c(0, 1, 1))
  s2$setInputs(x = 2)                                  # the dependency was registered: next event fires
  expect_identical(tally(runs2), c(1, 2, 2))
})
