context("sanitize-filename")

test_that("File names can't have multiple dots", {
  expect_equal(sanitizeFileName("../baz.jpg", "default"), ".baz.jpg")
  expect_equal(sanitizeFileName("./.baz.jpg", "default"), ".baz.jpg")
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

test_that("sanitizeFileName is vectorized", {
  names <- c("foo.txt", "", "bar.txt")
  defaults <- as.character(seq_along(names))
  expected <- c("foo.txt", "2", "bar.txt")
  expect_equal(sanitizeFileName(names, defaults), expected)
})

test_that("extension is preserved even when the name is not", {
  expect_equal(sanitizeFileName("你好.xlsx", "0"), "0.xlsx")
})

test_that("If the extension is preserved but combined with the default it's too long, use the default", {
  expect_equal(sanitizeFileName("你好.xlsx", "0", maxSize = 5), "0")
})

illegalNamesAndResults <- list(
  "COM1", "",
  "NUL.txt", ".txt",
  "NUL.COM1", ".COM1",
  ".NUL", ".NUL",
  "COM1LPT1NUL.jpg", "COM1LPT1NUL.jpg",
  c("NUL.COM1", "foo.txt"), c(".COM1", "foo.txt")
)

test_that("Illegal Windows file names are removed by the sanitize function when windows = TRUE", {
  for (idx in seq(1, length(illegalNamesAndResults), by = 2)) {
    expect_equal(sanitize(illegalNamesAndResults[[idx]], windows = TRUE), illegalNamesAndResults[[idx + 1]])
  }
})

test_that("Lower-case illegal Windows file names are removed by the sanitize function when windows = TRUE", {
  for (idx in seq(1, length(illegalNamesAndResults), by = 2)) {
    upName <- tolower(illegalNamesAndResults[[idx]])
    toResult <- tolower(illegalNamesAndResults[[idx + 1]])
    expect_equal(sanitize(upName, windows = TRUE), toResult)
  }
})

test_that("On Windows, illegal file names are removed", {
  skip_on_os("mac")
  skip_on_os("linux")
  skip_on_os("solaris")
  expect_equal(sanitizeFileName("COM1", "0"), "0")
  expect_equal(sanitizeFileName("NUL.txt", "0"), "0.txt")
})
