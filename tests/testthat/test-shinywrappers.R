context("shinywrappers")

test_that("isTempFile passes sanity checks", {
  expect_true(isTempFile(tempfile()))
  expect_false(isTempFile(path.expand("~")))
  expect_false(isTempFile("."))

  # Malformed temp dir isn't a problem
  expect_false(isTempFile(path.expand("~"), tempDir = ""))

  # Tolerant of trailing slashes
  expect_true(isTempFile("/foo/bar", tempDir = "/foo"))
  expect_true(isTempFile("/foo/bar", tempDir = "/foo/"))

  expect_false(isTempFile("/foo/bar", tempDir = "/foo/bar"))

  # Short filenames are OK
  expect_false(isTempFile("/foo", tempDir = "/foo/bar"))

  # path normalization
  expect_true(isTempFile(".", tempDir = normalizePath("..")))
  expect_true(isTempFile(normalizePath("."), tempDir = ".."))
  expect_true(isTempFile(".", tempDir = ".."))

  # not based on simple string matching
  expect_false(isTempFile("/foo/barbaz", tempDir = "/foo/bar"))
})
