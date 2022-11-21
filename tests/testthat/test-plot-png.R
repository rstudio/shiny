test_that("startPNG() throws informative error", {
  tmp <- tempfile(fileext = '.png')

  expect_error(
    startPNG(
      filename = tmp,
      width = NULL,
      height = 100,
      res = 72,
      shinyOutputId = "my_output"
    ),
    "PNG device `width` is length 0.*getCurrentOutputInfo()"
  )

  expect_error(
    startPNG(
      filename = tmp,
      width = 100,
      height = NULL,
      res = 72,
      shinyOutputId = "my_output"
    ),
    "PNG device `height` is length 0.*getCurrentOutputInfo()"
  )
})
