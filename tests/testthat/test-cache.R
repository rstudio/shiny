context("Cache")

test_that("DiskCache: handling missing values", {
  d <- diskCache()
  expect_true(is.key_missing(d$get("abcd")))
  # Can't add value that is identical to the sentinel value
  expect_error(d$set("x", key_missing()))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)

  d <- diskCache(missing = NULL)
  expect_true(is.null(d$get("abcd")))
  # Can't add value that is identical to the sentinel value
  expect_error(d$set("x", NULL))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)

  d <- diskCache(missing = quote(stop("Missing key")))
  expect_error(d$get("abcd"), "^Missing key$")
  # Unlike with sentinel values, with quoted expressions, we can set value that is
  # the same as missing key expression.
  d$set("x", quote(stop("Missing key")))
  expect_identical(d$get("x"), quote(stop("Missing key")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
})

test_that("MemoryCache: handling missing values", {
  d <- memoryCache()
  expect_true(is.key_missing(d$get("abcd")))
  # Can't add value that is identical to the sentinel value
  expect_error(d$set("x", key_missing()))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)

  d <- memoryCache(missing = NULL)
  expect_true(is.null(d$get("abcd")))
  # Can't add value that is identical to the sentinel value
  expect_error(d$set("x", NULL))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)

  d <- memoryCache(missing = quote(stop("Missing key")))
  expect_error(d$get("abcd"), "^Missing key$")
  # Unlike with sentinel values, with quoted expressions, we can set value that is
  # the same as missing key expression.
  d$set("x", quote(stop("Missing key")))
  expect_identical(d$get("x"), quote(stop("Missing key")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
})
