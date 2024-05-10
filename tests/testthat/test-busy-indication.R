test_that("useBusyIndicators()", {
  expect_snapshot(
    tagList(
      useBusyIndicators(),
      useBusyIndicators(spinners = FALSE),
      useBusyIndicators(pulse = FALSE),
      useBusyIndicators(spinners = FALSE, pulse = FALSE),
    )
  )

  expect_error(useBusyIndicators("foo"))
  expect_error(useBusyIndicators(foo = "bar"))
})

busy_indicator_options <- function(...) {
  withPrivateSeed(set.seed(100))
  busyIndicatorOptions(...)
}

test_that("busyIndicatorOptions()", {

  expect_snapshot(
    tagList(
      busy_indicator_options(),
      busy_indicator_options(spinner_type = "bars"),
      busy_indicator_options(spinner_type = "pulse"),
      busy_indicator_options(spinner_type = "dots"),
      busy_indicator_options(spinner_color = "red"),
      busy_indicator_options(spinner_size = "10px"),
      busy_indicator_options(spinner_delay = "1s"),
      busy_indicator_options(pulse_background = "blue"),
      busy_indicator_options(pulse_height = "10px"),
      busy_indicator_options(pulse_speed = "1s"),
      busy_indicator_options(
        spinner_color = "red",
        spinner_size = "10px",
        spinner_delay = "1s",
        pulse_background = "blue",
        pulse_height = "10px",
        pulse_speed = "1s"
      )
    )
  )

  expect_error(busy_indicator_options("foo"))
  expect_error(busy_indicator_options(foo = "bar"))
  expect_error(busy_indicator_options(spinner_type = "dsflds"))
  expect_error(busy_indicator_options(spinner_color = "dsflds"))
  expect_error(busy_indicator_options(spinner_size = "dsflds"))
  expect_error(busy_indicator_options(pulse_height = "dsflds"))
})


test_that("Can provide svg file for busyIndicatorOptions(spinner_type)", {
  skip_if(.Platform$OS.type == "windows")

  tmpsvg <- tempfile(fileext = ".svg")
  writeLines("<svg></svg>", tmpsvg)
  on.exit(unlink(tmpsvg))

  expect_snapshot(
    busy_indicator_options(spinner_type = tmpsvg)
  )
})
