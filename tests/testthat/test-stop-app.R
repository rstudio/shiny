checkAndGetResults <- function(appState, isError, isStopped) {
  stopifnot(appState$reterror == isError)
  stopifnot(appState$stopped, isStopped)
  appState$retval
}

test_that("stopApp records errors and respects visibility", {
  app <- shinyApp(
    ui = fluidPage(),
    server = function(input, output) {}
  )

  handle <- runApp(app, blocking = FALSE, launch.browser = FALSE, quiet = TRUE)
  on.exit(tryCatch(handle$stop(), error = function(e) NULL, warning = function(w) NULL), add = TRUE)

  appState <- shiny:::getCurrentAppState()

  stopApp(10)
  expect_equal(withVisible(10), checkAndGetResults(appState, FALSE, TRUE))

  # Reset for next test
  appState$stopped <- FALSE
  appState$retval <- NULL
  appState$reterror <- NULL

  stopApp(invisible(cars))
  expect_equal(withVisible(invisible(cars)), checkAndGetResults(appState, FALSE, TRUE))

  # Reset for next test
  appState$stopped <- FALSE
  appState$retval <- NULL
  appState$reterror <- NULL

  stopApp(stop("boom", call. = FALSE))
  err <- checkAndGetResults(appState, TRUE, TRUE)
  attr(err, "stack.trace") <- NULL
  expect_identical(simpleError("boom"), err)

  handle$stop()
})

test_that("stopApp is a no-op when no app is running", {
  expect_silent(stopApp(42))
})
