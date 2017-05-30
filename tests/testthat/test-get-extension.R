context("get-extension")

test_that("Valid extensions are preserved", {
  expect_equal(maybeGetExtension("report.csv"), ".csv")
  expect_equal(maybeGetExtension("data.tar.bz2"), ".bz2")
})

test_that("Invalid extensions are discarded", {
  expect_equal(maybeGetExtension("report. ℒ  裸 邏 ℓo "), "")
  expect_equal(maybeGetExtension("data"), "")
})
