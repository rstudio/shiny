test_that("startPNG() handles NULL dimensions sensibly", {
  tmp <- tempfile(fileext = '.png')
  plotPNG(function() plot(1), filename = tmp, width = NULL, height = NULL)
  bits <- readBin(tmp, "raw", file.info(tmp)$size)
  expect_true(length(bits) > 0)
})
