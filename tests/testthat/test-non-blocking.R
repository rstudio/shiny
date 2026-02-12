# Prevent browser launch in interactive sessions
withr::local_options(list(shiny.launch.browser = FALSE), .local_envir = teardown_env())

test_that("ShinyAppHandle lifecycle and API (success path)", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)

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

  # Double stop warns
  expect_warning(handle$stop(), "App is not running")
})

test_that("ShinyAppHandle lifecycle (error path)", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)

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

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)

  stopApp("test_result")
  while (handle$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }

  expect_equal(handle$status(), "success")
  expect_equal(handle$result(), "test_result")
})

test_that("blocking runApp prevented while another app is running", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_error(
    runApp(app2, blocking = TRUE, launch.browser = FALSE, quiet = TRUE),
    "Can't call blocking runApp"
  )

  handle$stop()
})

test_that("cleanup callbacks run when stopped", {
  stopped <- FALSE
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  onStop(function() stopped <<- TRUE)

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  handle$stop()

  expect_true(stopped)
})

test_that("old handle doesn't see new app's result", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)

  stopApp("result1")
  while (handle1$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }
  expect_equal(handle1$result(), "result1")

  # Start and stop app2
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)

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

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_true(isRunning())

  handle$stop()
  expect_false(isRunning())
})

test_that("shiny.blocking option controls default", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  withr::local_options(shiny.blocking = FALSE)

  handle <- runApp(app, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_s3_class(handle, "ShinyAppHandle")
  expect_equal(handle$status(), "running")

  handle$stop()
})

test_that("runExample works with blocking = FALSE", {
  handle <- suppressMessages(runExample("01_hello", blocking = FALSE, launch.browser = FALSE))
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_equal(handle$status(), "running")
  expect_match(handle$url(), "^http://")

  handle$stop()
  expect_equal(handle$status(), "success")
})

test_that("runGadget works with blocking = FALSE", {
  ui <- fluidPage(
    actionButton("done", "Done")
  )
  server <- function(input, output, session) {}

  handle <- suppressMessages(runGadget(ui, server, blocking = FALSE, viewer = function(url) NULL))
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_s3_class(handle, "ShinyAppHandle")
  expect_equal(handle$status(), "running")
  expect_match(handle$url(), "^http://")

  handle$stop()
  expect_equal(handle$status(), "success")
})

test_that("startup failure clears app state (regression test)", {
  # If startup fails after initCurrentAppState() but before earlyCleanup <- FALSE,
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
    runApp(failing_app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE),
    "Intentional startup failure"
  )

  # isRunning() should return FALSE - no app is actually running
  expect_false(isRunning())

  # A subsequent runApp() call should work
  working_app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(working_app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_equal(handle$status(), "running")
  handle$stop()
})

# ============================================================================
# Multi-app concurrent tests
# ============================================================================

test_that("two concurrent non-blocking apps on different ports", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_equal(handle1$status(), "running")
  expect_equal(handle2$status(), "running")

  # Different URLs (different ports)
  expect_false(identical(handle1$url(), handle2$url()))

  handle1$stop()
  handle2$stop()

  expect_equal(handle1$status(), "success")
  expect_equal(handle2$status(), "success")
})

test_that("stop one app while other keeps running", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Stop app1
  handle1$stop()
  expect_equal(handle1$status(), "success")

  # app2 still running
  expect_equal(handle2$status(), "running")

  handle2$stop()
  expect_equal(handle2$status(), "success")
})

test_that("independent onStop callbacks per app", {
  stopped1 <- FALSE
  stopped2 <- FALSE

  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() {
      onStop(function() stopped1 <<- TRUE)
    }
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() {
      onStop(function() stopped2 <<- TRUE)
    }
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Stop only app1
  handle1$stop()
  expect_true(stopped1)
  expect_false(stopped2)

  # Stop app2
  handle2$stop()
  expect_true(stopped2)
})

test_that("independent onUnhandledError callbacks per app", {
  errors1 <- list()
  errors2 <- list()

  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() {
      onUnhandledError(function(e) errors1[[length(errors1) + 1L]] <<- e)
    }
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    onStart = function() {
      onUnhandledError(function(e) errors2[[length(errors2) + 1L]] <<- e)
    }
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Invoke app1's onUnhandledError callbacks directly
  appState1 <- handle1$.__enclos_env__$private$appState
  appState1$onUnhandledErrorCallbacks$invoke(simpleError("err1"))

  expect_length(errors1, 1)
  expect_equal(conditionMessage(errors1[[1]]), "err1")
  expect_length(errors2, 0)

  # Invoke app2's onUnhandledError callbacks directly
  appState2 <- handle2$.__enclos_env__$private$appState
  appState2$onUnhandledErrorCallbacks$invoke(simpleError("err2"))

  expect_length(errors2, 1)
  expect_equal(conditionMessage(errors2[[1]]), "err2")
  # app1 still only has its original error
  expect_length(errors1, 1)

  handle1$stop()
  handle2$stop()
})

test_that("independent stopApp results per app", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Stop apps via handle (direct)
  handle1$stop()
  handle2$stop()

  expect_equal(handle1$status(), "success")
  expect_equal(handle2$status(), "success")
})

test_that("isRunning() with multiple apps", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  expect_false(isRunning())

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)
  expect_true(isRunning())

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)
  expect_true(isRunning())

  # Stop one — still running
  handle1$stop()
  expect_true(isRunning())

  # Stop both — not running
  handle2$stop()
  expect_false(isRunning())
})

test_that("global onStop callbacks fire when last app stops", {
  globalStopped <- FALSE

  # Register global callback before any app starts
  onStop(function() globalStopped <<- TRUE)

  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Stop first app — global callback should NOT fire yet
  handle1$stop()
  expect_false(globalStopped)

  # Stop second (last) app — global callback should fire
  handle2$stop()
  expect_true(globalStopped)
})

test_that("stopping one app doesn't affect the other", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Stop app1 via handle (deterministic)
  handle1$stop()

  expect_equal(handle1$status(), "success")

  # app2 should still be running
  expect_equal(handle2$status(), "running")

  handle2$stop()
  expect_equal(handle2$status(), "success")
})

# ============================================================================
# Coverage gap tests
# ============================================================================

test_that("stopApp(value) targets getCurrentAppState() in multi-app", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # After starting both, getCurrentAppState() points to app2 (most recently
  # initialized), so stopApp() targets app2
  stopApp("targeted_result")
  while (handle2$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }

  expect_equal(handle2$result(), "targeted_result")
  expect_equal(handle1$status(), "running")

  # After app2 cleanup, currentAppState is NULL (cleared because it matched
  # app2's token). stopApp() is now a no-op.
  stopApp("should_be_ignored")
  later::run_now(timeoutSecs = 0.1)
  expect_equal(handle1$status(), "running")

  handle1$stop()
})

test_that("stopApp(value) targets explicitly-set app in multi-app", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Simulate what HTTP/WS context handlers do: point to app1
  appState1 <- handle1$.__enclos_env__$private$appState
  globals <- shiny:::.globals
  globals$currentAppState <- appState1

  stopApp("result_for_app1")
  while (handle1$status() == "running") {
    later::run_now(timeoutSecs = 1)
  }

  expect_equal(handle1$result(), "result_for_app1")
  expect_equal(handle2$status(), "running")

  handle2$stop()
})

test_that("process options saved on first app, restored when last stops", {
  withr::local_options(warn = 0)

  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # First app raises warn to 1
  expect_equal(getOption("warn"), 1)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Second app doesn't re-snapshot
  expect_equal(getOption("warn"), 1)

  # Stopping one app doesn't restore
  handle1$stop()
  expect_equal(getOption("warn"), 1)

  # Stopping last app restores original
  handle2$stop()
  expect_equal(getOption("warn"), 0)
})

test_that("serviceLoop attributes serviceApp() error to current app", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # Mock serviceApp to throw, simulating an httpuv-level error
  local_mocked_bindings(serviceApp = function() stop("service_loop_error"))

  while (handle$status() == "running") {
    later::run_now(timeoutSecs = 0.5)
  }

  expect_equal(handle$status(), "error")
  expect_error(handle$result(), "service_loop_error")
})

test_that("blocking option can be set via shinyApp(options)", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {},
    options = list(blocking = FALSE)
  )

  handle <- runApp(app, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_s3_class(handle, "ShinyAppHandle")
  expect_equal(handle$status(), "running")

  handle$stop()
})

test_that("stopping non-current app preserves currentAppState pointer", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle1$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle2$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # currentAppState points to app2 (most recently initialized)
  appState2 <- handle2$.__enclos_env__$private$appState
  expect_identical(shiny:::getCurrentAppState(), appState2)

  # Stop app1 — its token doesn't match currentAppState
  handle1$stop()

  # Pointer should still reference app2
  expect_identical(shiny:::getCurrentAppState(), appState2)

  handle2$stop()
})
