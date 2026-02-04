# Prevent browser launch in interactive sessions
withr::local_options(list(shiny.launch.browser = FALSE), .local_envir = teardown_env())

test_that("non-blocking app starts and stops", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  expect_true(handle$isRunning())
  expect_match(handle$getUrl(), "^http://")
  expect_s3_class(handle$getServer(), "Server")

  handle$stop()

  expect_false(handle$isRunning())
})

test_that("second app prevented while first is running", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app1, blocking = FALSE, launch.browser = FALSE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_error(
    runApp(app2, blocking = FALSE, launch.browser = FALSE),
    "Can't start a new app while another is running"
  )

  handle$stop()

  # After stopping, should be able to start a new app
  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE)
  expect_true(handle2$isRunning())
  handle2$stop()
})

test_that("cleanup runs when handle is stopped", {
  stopped <- FALSE
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  onStop(function() stopped <<- TRUE)

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)
  handle$stop()

  expect_true(stopped)
})

test_that("stopping already-stopped handle gives warning", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)
  handle$stop()

  expect_warning(handle$stop(), "App is not running")
})

test_that("ShinyAppHandle print method works", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  output <- capture.output(print(handle))
  expect_match(output[1], "Shiny app handle")
  expect_match(output[2], "URL:")
  expect_match(output[3], "running")

  handle$stop()

  output <- capture.output(print(handle))
  expect_match(output[3], "stopped")
})

test_that("handle captures result from external stopApp call", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  # Simulate stopApp being called (e.g., from within app code)
  stopApp("test_result")

  # Service the app to pick up the stopped state (100ms service loop delay)
  for (i in 1:10) {
    if (!handle$isRunning()) break
    later::run_now(timeoutSecs = 0.1)
  }

  expect_false(handle$isRunning())
  expect_equal(handle$result(), "test_result")
  expect_null(handle$error())
})

test_that("handle captures error from external stopApp call", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  # Simulate stopApp being called with an error
  stopApp(stop("test_error", call. = FALSE))

  # Service the app to pick up the stopped state
  for (i in 1:10) {
    if (!handle$isRunning()) break
    later::run_now(timeoutSecs = 0.1)
  }

  expect_false(handle$isRunning())
  expect_null(handle$result())
  err <- handle$error()
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "test_error")
})

test_that("old handle doesn't see new app's result", {
  app1 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle1 <- runApp(app1, blocking = FALSE, launch.browser = FALSE)

  # Stop app1 with result1
  stopApp("result1")
  for (i in 1:5) {
    if (!handle1$isRunning()) break
    later::run_now(timeoutSecs = 0.1)
  }
  expect_equal(handle1$result(), "result1")
  expect_false(handle1$isRunning())

  # Start app2
  app2 <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )
  handle2 <- runApp(app2, blocking = FALSE, launch.browser = FALSE)

  # Stop app2 with result2
  stopApp("result2")
  for (i in 1:5) {
    if (!handle2$isRunning()) break
    later::run_now(timeoutSecs = 0.1)
  }
  expect_equal(handle2$result(), "result2")

  # Critical: handle1 should still have its original result, not handle2's
  expect_equal(handle1$result(), "result1")
})

test_that("runExample works with blocking = FALSE", {
  handle <- runExample("01_hello", blocking = FALSE, launch.browser = FALSE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_true(handle$isRunning())
  expect_match(handle$getUrl(), "^http://")

  handle$stop()
  expect_false(handle$isRunning())
})

test_that("isRunning global check works correctly", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  # Before starting app
  expect_false(isRunning())

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  # While app is running
  expect_true(isRunning())

  handle$stop()

  # After stopping app
  expect_false(isRunning())
})

test_that("result and error are NULL before stopApp is called", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  # Before any stopApp call, both should be NULL

  expect_null(handle$result())
  expect_null(handle$error())

  handle$stop()

  # After manual stop (no stopApp value), still NULL
  expect_null(handle$result())
  expect_null(handle$error())
})

test_that("handle$stop returns invisible self",
{
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  # stop() should return invisible(self) for chaining
  ret <- withVisible(handle$stop())
  expect_false(ret$visible)
  expect_identical(ret$value, handle)
})

test_that("shiny.blocking option controls default", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  withr::local_options(shiny.blocking = FALSE)

  # Should return handle when option is FALSE

  handle <- runApp(app, launch.browser = FALSE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)


  expect_s3_class(handle, "ShinyAppHandle")
  expect_true(handle$isRunning())

  handle$stop()
})

test_that(".captureResult is idempotent", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE)

  stopApp("first_result")
  for (i in 1:10) {
    if (!handle$isRunning()) break
    later::run_now(timeoutSecs = 0.1)
  }

  expect_equal(handle$result(), "first_result")

 # Calling .captureResult again should be a no-op (early return)
  .globals <- shiny:::.globals
  .globals$retval <- list(value = "second_result")
  handle$.captureResult()

  # Result should still be first_result, not second_result
  expect_equal(handle$result(), "first_result")
})

test_that("runGadget works with blocking = FALSE", {
  ui <- fluidPage(
    actionButton("done", "Done")
  )
  server <- function(input, output, session) {}

  handle <- runGadget(ui, server, blocking = FALSE, viewer = function(url) NULL)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  expect_s3_class(handle, "ShinyAppHandle")
  expect_true(handle$isRunning())
  expect_match(handle$getUrl(), "^http://")

  handle$stop()
  expect_false(handle$isRunning())
})
