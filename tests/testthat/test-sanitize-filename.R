context("sanitize-filename")

test_that("File names can't have multiple dots", {
  expect_equal(sanitizeFileName("../baz.jpg", "default"), ".baz.jpg")
})

test_that("If the file name is longer than 255 characters, the default name is returned", {
  longName <- paste(replicate(256, "x"), collapse = "")
  expect_equal(sanitizeFileName(longName, "default"), "default")
})

test_that("If the file name is empty, the default name is returned", {
  expect_equal(sanitizeFileName("", "default"), "default")
})

test_that("Sanitized file names contain only alphanumeric characters and dots", {
  crazyString <- " ℍ ℎ f ℏ ▅ ▆ ▇ ℐ oℑ ℒ  裸 邏 ℓo ⌔ ⌕ ⌖b ⌗ ⌘ar.csv"
  expect_equal(sanitizeFileName(crazyString, "default"), "foobar.csv")
})

test_that("If the file name is empty after being sanitized, the default is returned", {
  expect_equal(sanitizeFileName("ℍℍℍℍ ", "default"), "default")
})
