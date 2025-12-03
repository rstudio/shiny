# Dev CRAN had some issues with comparisons between integer(0) and character(0)
# Skipping tests on CRAN as CI is enough to verify functionality
# Related: https://github.com/rstudio/shiny/issues/4326
skip_on_cran()

test_that("Scheduling works", {
  ran <- FALSE
  fun <- function() {
    ran <<- TRUE
  }

  timerCallbacks$schedule(500, fun)

  timerCallbacks$executeElapsed()
  expect_false(ran)

  Sys.sleep(0.1)
  timerCallbacks$executeElapsed()
  expect_false(ran)

  Sys.sleep(0.5)
  expect_true(timerCallbacks$executeElapsed())
  expect_true(ran)

  # Empty timerCallbacks should return FALSE
  expect_false(timerCallbacks$executeElapsed())
  expect_equal(0, nrow(timerCallbacks$takeElapsed()))
})

test_that("Unscheduling works", {
  origTimes <- timerCallbacks$.times
  origFuncKeys <- timerCallbacks$.funcs$keys()

  taskHandle <- scheduleTask(1000, function() {
    message("Whatever")
  })
  # Unregister
  taskHandle()

  # Split into two sections to avoid `expect_equal(integer(0), character(0))` comparison on dev CRAN
  if (length(origTimes) == 0) {
    expect_equal(0, length(timerCallbacks$.times))
  } else {
    expect_equal(timerCallbacks$.times, origTimes)
  }
  expect_equal(timerCallbacks$.funcs$keys(), origFuncKeys)
})

test_that("Vectorized unscheduling works", {
  key1 <- timerCallbacks$schedule(1000, function() {})
  key2 <- timerCallbacks$schedule(1000, function() {})
  key3 <- timerCallbacks$schedule(1000, function() {})

  expect_identical(timerCallbacks$unschedule(key2), TRUE)
  expect_identical(timerCallbacks$unschedule(c(key1, key2, key3)), c(TRUE, FALSE, TRUE))
})

test_that("defineScheduler works", {
  expect_identical(defineScheduler(NULL), scheduleTask)
  expect_identical(defineScheduler(list()), scheduleTask)
  expect_identical(defineScheduler(list(.scheduleTask=123)), 123)
})

test_that("mockableTimer works", {
  mt <- MockableTimerCallbacks$new()
  called <- FALSE
  mt$schedule(50, function(){
    called <<- TRUE
  })
  expect_false(mt$executeElapsed())

  # Prove that we're not bound to a real clock
  Sys.sleep(.1)
  expect_false(mt$executeElapsed())
  expect_false(called)

  mt$elapse(51)
  expect_true(mt$executeElapsed())
  expect_true(called)
})

test_that("getDomainTimeMs works", {
  start <- as.numeric(Sys.time()) * 1000
  t1 <- getDomainTimeMs(NULL)
  t2 <- getDomainTimeMs(list())
  t3 <- getDomainTimeMs(list(.now = function(){456}))
  end <- as.numeric(Sys.time()) * 1000

  expect_gte(t1, start)
  expect_gte(t2, start)
  expect_lte(t1, end)
  expect_lte(t2, end)

  expect_equal(t3, 456)
})
