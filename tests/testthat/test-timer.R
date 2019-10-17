context("timer")

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

  expect_identical(timerCallbacks$.times, origTimes)
  expect_identical(timerCallbacks$.funcs$keys(), origFuncKeys)
})

test_that("Vectorized unscheduling works", {
  key1 <- timerCallbacks$schedule(1000, function() {})
  key2 <- timerCallbacks$schedule(1000, function() {})
  key3 <- timerCallbacks$schedule(1000, function() {})

  expect_identical(timerCallbacks$unschedule(key2), TRUE)
  expect_identical(timerCallbacks$unschedule(c(key1, key2, key3)), c(TRUE, FALSE, TRUE))
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
