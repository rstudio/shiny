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

test_that("busyIndicatorOptions()", {
  withPrivateSeed(set.seed(100))

  expect_snapshot(
    tagList(
      busyIndicatorOptions(),
      busyIndicatorOptions(spinner_type = "bars"),
      busyIndicatorOptions(spinner_type = "pulse"),
      busyIndicatorOptions(spinner_type = "dots"),
      busyIndicatorOptions(spinner_color = "red"),
      busyIndicatorOptions(spinner_size = "10px"),
      busyIndicatorOptions(spinner_delay = "1s"),
      busyIndicatorOptions(pulse_background = "blue"),
      busyIndicatorOptions(pulse_height = "10px"),
      busyIndicatorOptions(pulse_speed = "1s"),
      busyIndicatorOptions(
        spinner_color = "red",
        spinner_size = "10px",
        spinner_delay = "1s",
        pulse_background = "blue",
        pulse_height = "10px",
        pulse_speed = "1s"
      )
    )
  )

  expect_error(busyIndicatorOptions("foo"))
  expect_error(busyIndicatorOptions(foo = "bar"))
  expect_error(busyIndicatorOptions(spinner_type = "dsflds"))
  expect_error(busyIndicatorOptions(spinner_color = "dsflds"))
  expect_error(busyIndicatorOptions(spinner_size = "dsflds"))
  expect_error(busyIndicatorOptions(pulse_height = "dsflds"))
})


test_that("Can provide svg file for busyIndicatorOptions(spinner_type)", {
  skip_on_os("windows")

  tmpsvg <- tempfile(fileext = ".svg")
  writeLines("<svg></svg>", tmpsvg)
  on.exit(unlink(tmpsvg))

  withPrivateSeed(set.seed(100))
  expect_snapshot(
    busyIndicatorOptions(spinner_type = tmpsvg)
  )
})
