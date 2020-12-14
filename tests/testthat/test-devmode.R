
test_that("devmode is not on while testing", {
  expect_equal(in_devmode(), FALSE)
})

test_that("devmode can not be turned on while testing", {
  with_devmode(TRUE, {
    expect_equal(in_devmode(), FALSE)
  })
})

test_that("devmode can be turned on while _testing_ is disabled and check messages", {

  # disable all existing options
  withr::local_options(list(
    shiny.devmode = NULL,
    shiny.devmode.verbose = NULL,
    shiny.autoreload = NULL,
    shiny.minified = NULL,
    shiny.fullstacktrace = NULL
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

    # if in devmode...

    capture_message <- function(code) {
      ret <- NULL
      withCallingHandlers(
        code,
        message = function(m) {
          ret <<- m$message
        }
      )
      ret
    }

    expect_inform <- function(code, msg) {
      msg_output <- capture_message(code)
      # due to inform not always signaling as `.frequency = "regularly"`...
      if (length(msg_output) > 0) {
        expect_match(msg_output, msg, fixed = TRUE)
      }
    }

    expect_inform(
      devmode_inform("custom message here"),
      "shiny devmode - "
    )

    expect_devmode_option <- function(key, val, msg, ...) {
      expect_inform(
        {
          devmode_val <- get_devmode_option(key, "default", ...)
        },
        msg
      )
      expect_equal(devmode_val, val)
    }

    expect_devmode_option("shiny.autoreload", TRUE, "Turning on shiny autoreload")
    expect_devmode_option("shiny.minified", FALSE, "Using full shiny javascript file")
    expect_devmode_option("shiny.fullstacktrace", TRUE, "Turning on full stack trace")

    # local values can be overwritten
    expect_devmode_option("shiny.autoreload", "on", "Turning on shiny autoreload", on = "on")
    expect_devmode_option("shiny.autoreload", TRUE, "custom msg", message = "custom msg")

  })

})


test_that("no devmode side effects from testing", {
  expect_equal(in_devmode(), FALSE)
})
