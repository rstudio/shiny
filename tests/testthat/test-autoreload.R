test_that("initAutoReloadMonitor() does nothing when autoreload is off", {
  withr::local_options(shiny.autoreload = FALSE)
  local_mocked_bindings(
    is_installed = function(...) stop("should not be called"),
    .package = "rlang"
  )

  stop_monitor <- initAutoReloadMonitor(tempdir())
  expect_type(stop_monitor, "closure")
  expect_silent(stop_monitor())
})

test_that("initAutoReloadMonitor() warns and keeps running without {watcher}", {
  withr::local_options(shiny.autoreload = TRUE)
  local_mocked_bindings(
    is_installed = function(...) FALSE,
    # Same as a non-interactive session, or a user declining the prompt
    check_installed = function(...) rlang::abort("not installed"),
    .package = "rlang"
  )

  expect_warning(
    stop_monitor <- initAutoReloadMonitor(tempdir()),
    "watcher"
  )
  expect_type(stop_monitor, "closure")
  expect_silent(stop_monitor())
})

test_that("initAutoReloadMonitor() starts {watcher} once it is installed", {
  skip_if_not_installed("watcher", "0.2.0")
  withr::local_options(shiny.autoreload = TRUE)
  local_mocked_bindings(
    is_installed = function(...) FALSE,
    # Same as a user accepting the prompt and installing it
    check_installed = function(...) invisible(NULL),
    .package = "rlang"
  )
  started <- FALSE
  local_mocked_bindings(
    watcher = function(path, callback, latency) {
      list(
        start = function() started <<- TRUE,
        stop = function() started <<- FALSE
      )
    },
    .package = "watcher"
  )

  stop_monitor <- initAutoReloadMonitor(tempdir())
  expect_true(started)
  stop_monitor()
  expect_false(started)
})
