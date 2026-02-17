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

  # Double stop is a silent no-op
  expect_no_warning(handle$stop())
  expect_equal(handle$status(), "success")
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

test_that("non-blocking auto-stops previous app when starting new one", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  expect_equal(handle1$status(), "running")

  # Starting a second non-blocking app should auto-stop the first
  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(handle2$stop(), add = TRUE)

  expect_equal(handle1$status(), "success")
  expect_equal(handle2$status(), "running")

  handle2$stop()
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
      runApp(inner_app, blocking = TRUE, launch.browser = FALSE, quiet = TRUE)
    }
  )

  expect_error(
    runApp(outer_app, blocking = TRUE, launch.browser = FALSE, quiet = TRUE),
    "Can't start a new app while another is running"
  )
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
  on.exit(handle$stop(), add = TRUE)

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
  on.exit(handle$stop(), add = TRUE)

  expect_s3_class(handle, "ShinyAppHandle")
  expect_equal(handle$status(), "running")

  handle$stop()
})

test_that("runExample works with blocking = FALSE", {
  handle <- suppressMessages(runExample("01_hello", blocking = FALSE, launch.browser = FALSE))
  on.exit(handle$stop(), add = TRUE)

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
  on.exit(handle$stop(), add = TRUE)

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
  on.exit(handle$stop(), add = TRUE)

  expect_equal(handle$status(), "running")
  handle$stop()
})
