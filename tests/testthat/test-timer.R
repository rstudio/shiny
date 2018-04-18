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
