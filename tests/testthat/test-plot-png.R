test_that("plotPNG()/startPNG() ignores NULL dimensions", {
  f <- plotPNG(function() plot(1), width = NULL, height = NULL)
  on.exit(unlink(f))
  bits <- readBin(f, "raw", file.info(f)$size)
  expect_gt(length(bits), 0)
})
