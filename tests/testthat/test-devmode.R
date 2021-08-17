
test_that("devmode is not on while testing", {
  expect_equal(in_devmode(), FALSE)
})

test_that("devmode can not be turned on while testing", {
  with_devmode(TRUE, {
    expect_equal(in_devmode(), FALSE)
  })
})

test_that("devmode can be turned on while _testing_ is disabled and check messages", {

  # TODO - Once a stable version of rlang lands with `options(rlib_message_verbosity)` functionality,
  #        bump the rlang version in DESCRIPTION and remove these skip calls
  # Skip if *not* on CI; Inspired from `testthat::skip_on_ci()`
  skip_if(!isTRUE(as.logical(Sys.getenv("CI"))), "Not testing in CI")
  skip_if_not_installed("rlang", "0.4.11.9000")

  # disable all existing options
  withr::local_options(list(
    shiny.devmode = NULL,
    shiny.devmode.verbose = NULL,
    shiny.autoreload = NULL,
    shiny.minified = NULL,
    shiny.fullstacktrace = NULL,
    # force inform to always generate signal
    rlib_message_verbosity = "verbose"
  ))
  # "turn off" testing to allow for devmode to activate
  withr::local_envvar(list(
    TESTTHAT = "false"
  ))

  expect_equal(getOption("shiny.devmode", "default"), "default")
  expect_equal(getOption("shiny.devmode.verbose", "default"), "default")
  expect_equal(get_devmode_option("shiny.autoreload", "default"), "default")
  expect_equal(get_devmode_option("shiny.minified", "default"), "default")
  expect_equal(get_devmode_option("shiny.fullstacktrace", "default"), "default")

  with_devmode(TRUE, verbose = TRUE, {
    expect_equal(in_devmode(), TRUE)
    expect_message(devmode_inform("custom message here"), "shiny devmode - ")

    expect_devmode_option <- function(key, expected, msg, ...) {
      expect_message(val <- get_devmode_option(key, ...), msg)
      expect_equal(val, expected)
    }

    expect_devmode_option("shiny.autoreload", TRUE, "Turning on shiny autoreload")
    expect_devmode_option("shiny.minified", FALSE, "Using full shiny javascript file")
    expect_devmode_option("shiny.fullstacktrace", TRUE, "Turning on full stack trace")

    # local values can be overwritten
    expect_devmode_option("shiny.autoreload", "on", "Turning on shiny autoreload", devmode_default = "on")
    expect_devmode_option("shiny.autoreload", TRUE, "custom msg", devmode_message = "custom msg")

  })

})


test_that("no devmode side effects from testing", {
  expect_equal(in_devmode(), FALSE)
})
