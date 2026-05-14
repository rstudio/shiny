# Prevent browser launch in interactive sessions
withr::local_options(list(shiny.launch.browser = FALSE), .local_envir = teardown_env())

test_that("ShinyAppHandle lifecycle and API (success path)", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- startApp(app, launch.browser = FALSE, quiet = TRUE)

  # While running

  expect_equal(handle$status(), "running")
  expect_match(handle$url(), "^http://")
  expect_error(handle$result(), "App is still running")

  output <- capture.output(print(handle))
  expect_match(output[1], "Shiny app handle")
  expect_match(output[2], "URL:")
  expect_match(output[3], "running")

  # stop() returns invisible self
  ret <- withVisible(handle$stop())
  expect_false(ret$visible)
  expect_identical(ret$value, handle)

  # After stop
  expect_equal(handle$status(), "success")
  expect_null(handle$result())

  output <- capture.output(print(handle))
  expect_match(output[3], "success")

  # Double stop is a silent no-op
  expect_no_warning(handle$stop())
  expect_equal(handle$status(), "success")
})

test_that("ShinyAppHandle lifecycle (error path)", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- startApp(app, launch.browser = FALSE, quiet = TRUE)

  stopApp(stop("test_error", call. = FALSE))
  while (handle$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }

  expect_equal(handle$status(), "error")
  expect_error(handle$result(), "test_error")

  output <- capture.output(print(handle))
  expect_match(output[3], "error")
})

test_that("handle captures result from stopApp", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- startApp(app, launch.browser = FALSE, quiet = TRUE)

  stopApp("test_result")
  while (handle$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }

  expect_equal(handle$status(), "success")
  expect_equal(handle$result(), "test_result")
})

test_that("non-blocking auto-stops previous app when starting new one", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- startApp(app1, launch.browser = FALSE, quiet = TRUE)
  expect_equal(handle1$status(), "running")

  # Starting a second non-blocking app should auto-stop the first
  handle2 <- startApp(app2, launch.browser = FALSE, quiet = TRUE)
  on.exit(handle2$stop(), add = TRUE)

  expect_equal(handle1$status(), "success")
  expect_equal(handle2$status(), "running")

  handle2$stop()
})

test_that("replacing a non-blocking app does not leave stale service loops", {
  generations_seen <- integer(0)

  # Mock serviceApp to record which generation is active when called
  local_mocked_bindings(
    serviceApp = function(timeout) {
      generations_seen[[length(generations_seen) + 1L]] <<-
        .globals$serviceGeneration
    },
    .package = "shiny"
  )

  app1 <- shinyApp(ui = fluidPage(), server = function(input, output) {})
  app2 <- shinyApp(ui = fluidPage(), server = function(input, output) {})

  handle1 <- startApp(app1, launch.browser = FALSE, quiet = TRUE)
  gen1 <- .globals$serviceGeneration

  handle2 <- startApp(app2, launch.browser = FALSE, quiet = TRUE)
  on.exit(handle2$stop(), add = TRUE)
  gen2 <- .globals$serviceGeneration

  # Reset and let service loops run
  generations_seen <- integer(0)
  while (length(generations_seen) < 5L) later::run_now(timeoutSecs = 1)

  # Only the new generation should be servicing
  expect_true(length(generations_seen) > 0)
  expect_true(all(generations_seen == gen2))

  handle2$stop()
})

test_that("starting a blocking app invalidates stale non-blocking service loops", {
  service_calls <- 0L

  local_mocked_bindings(
    serviceApp = function(timeout) {
      service_calls <<- service_calls + 1L
    },
    .package = "shiny"
  )

  ns <- asNamespace("shiny")
  g <- get(".globals", envir = ns)

  # Simulate a non-blocking app at generation 1
  assign("serviceGeneration", 1L, envir = g)
  assign("stopped", FALSE, envir = g)
  shiny:::serviceNonBlocking(list(stop = function() {}), 1L)

  # Simulate stopping app 1, then starting a blocking app which bumps generation
  assign("stopped", TRUE, envir = g)
  assign("serviceGeneration", 2L, envir = g)
  assign("stopped", FALSE, envir = g)

  later::run_now(timeoutSecs = 1)

  expect_equal(service_calls, 0L)
})

test_that("nested runApp in blocking mode still errors", {
  inner_app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  outer_app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() {
      runApp(inner_app, launch.browser = FALSE, quiet = TRUE)
    }
  )

  expect_error(
    runApp(outer_app, launch.browser = FALSE, quiet = TRUE),
    "from within `runApp"
  )
})

test_that("nested startApp from within a tick errors instead of auto-replacing", {
  app1 <- shinyApp(ui = fluidPage(), server = function(input, output) {})
  app2 <- shinyApp(ui = fluidPage(), server = function(input, output) {})

  handle <- startApp(app1, launch.browser = FALSE, quiet = TRUE)
  on.exit({
    h <- shiny:::.globals$runningHandle
    if (!is.null(h)) h$stop()
  }, add = TRUE)

  # `serviceApp(NA)` drains one pending later callback while on the stack,
  # mirroring production where user code runs inside the tick.
  in_tick <- NULL
  nested_err <- NULL
  later::later(function() {
    in_tick <<- shiny:::.isInAppTick()
    nested_err <<- tryCatch(
      startApp(app2, launch.browser = FALSE, quiet = TRUE),
      error = identity
    )
  })
  while (is.null(nested_err)) later::run_now(timeoutSecs = 1)

  expect_true(in_tick)
  expect_s3_class(nested_err, "error")
  expect_match(conditionMessage(nested_err), "another is running")
  expect_equal(handle$status(), "running")
})

test_that("cleanup callbacks run when stopped", {
  stopped <- FALSE
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  onStop(function() stopped <<- TRUE)

  handle <- startApp(app, launch.browser = FALSE, quiet = TRUE)
  handle$stop()

  expect_true(stopped)
})

test_that("old handle doesn't see new app's result", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- startApp(app1, launch.browser = FALSE, quiet = TRUE)

  stopApp("result1")
  while (handle1$status() == "running") {
    later::run_now(1)
  }
  expect_equal(handle1$result(), "result1")

  # Start and stop app2
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  handle2 <- startApp(app2, launch.browser = FALSE, quiet = TRUE)

  stopApp("result2")
  while (handle2$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }
  expect_equal(handle2$result(), "result2")

  # handle1 should still have its original result
  expect_equal(handle1$result(), "result1")
})

test_that("global isRunning() works with non-blocking apps", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  expect_false(isRunning())

  handle <- startApp(app, launch.browser = FALSE, quiet = TRUE)
  on.exit(handle$stop(), add = TRUE)

  expect_true(isRunning())

  handle$stop()
  expect_false(isRunning())
})

test_that("startup failure clears app state (regression test)", {
  # If startup fails after initCurrentAppState() but before cleanupOnExit <- FALSE,
  # the app state must be cleared so subsequent runApp() calls don't fail with
  # "Can't start a new app while another is running"

  # Create an app that fails during onStart (which runs after initCurrentAppState)
  failing_app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() stop("Intentional startup failure")
  )

  # This should fail
  expect_error(
    startApp(failing_app, launch.browser = FALSE, quiet = TRUE),
    "Intentional startup failure"
  )

  # isRunning() should return FALSE - no app is actually running
  expect_false(isRunning())

  # A subsequent runApp() call should work
  working_app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- startApp(working_app, launch.browser = FALSE, quiet = TRUE)
  on.exit(handle$stop(), add = TRUE)

  expect_equal(handle$status(), "running")
  handle$stop()
})

test_that("stopApp() called from onStart is preserved during setup", {
  # Regression: .setupShinyApp() previously reset the stop globals after
  # onStart() ran, silently clobbering a stop request made during setup.

  app_ok <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() stopApp("early_result")
  )

  # Blocking: short-circuits the service loop and returns the value.
  expect_equal(
    runApp(app_ok, launch.browser = FALSE, quiet = TRUE),
    "early_result"
  )
  expect_false(isRunning())

  # Non-blocking: handle is in its terminal state on return — no service
  # tick required, since the stop was already requested during setup.
  handle <- startApp(app_ok, launch.browser = FALSE, quiet = TRUE)
  expect_equal(handle$status(), "success")
  expect_equal(handle$result(), "early_result")
  expect_false(isRunning())

  # Non-blocking error path: the condition reaches handle$result().
  app_err <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() stopApp(stop("early_error", call. = FALSE))
  )
  handle <- startApp(app_err, launch.browser = FALSE, quiet = TRUE)
  expect_equal(handle$status(), "error")
  expect_error(handle$result(), "early_error")
  expect_false(isRunning())
})
