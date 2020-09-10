context("Cache")

test_that("DiskCache: handling missing values", {
  d <- diskCache()
  expect_true(is.key_missing(d$get("abcd")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = NULL), NULL)
  expect_error(
    d$get("y", missing = function(key) stop("Missing key: ", key), exec_missing = TRUE),
    "^Missing key: y$",
  )

  d <- diskCache(missing = NULL)
  expect_true(is.null(d$get("abcd")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = -1), -1)
  expect_error(
    d$get("y", missing = function(key) stop("Missing key: ", key), exec_missing = TRUE),
    "^Missing key: y$",
  )


  d <- diskCache(missing = function(key) stop("Missing key: ", key), exec_missing = TRUE)
  expect_error(d$get("abcd"), "^Missing key: abcd$")
  # When exec_missing=TRUE, should be able to set a value that's identical to
  # missing. Need to suppress warnings, because it will warn about reference
  # object (the environment captured by the function)
  d$set("x", NULL)
  suppressWarnings(d$set("x", function(key) stop("Missing key: ", key)))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = NULL, exec_missing = FALSE), NULL)
  expect_true(is.key_missing(d$get("y", missing = key_missing(), exec_missing = FALSE)))
  expect_equal(d$get("y", exec_missing = FALSE), function(key) stop("Missing key: ", key))
  expect_error(
    d$get("y", missing = function(key) stop("Missing key 2: ", key), exec_missing = TRUE),
    "^Missing key 2: y$",
  )

  # Can't use exec_missing when missing is not a function
  expect_error(diskCache(missing = 1, exec_missing = TRUE))
})

# Issue #3033
test_that("DiskCache: pruning respects both max_n and max_size", {
  d <- diskCache(max_n = 3, max_size = 200)
  # Set some values. Use rnorm so that object size is large; a simple vector
  # like 1:100 will be stored very efficiently by R's ALTREP, and won't exceed
  # the max_size. We want each of these objects to exceed max_size so that
  # they'll be pruned.
  d$set("a", rnorm(100))
  d$set("b", rnorm(100))
  d$set("c", rnorm(100))
  d$set("d", rnorm(100))
  d$set("e", rnorm(100))
  Sys.sleep(0.1)  # For systems that have low mtime resolution.
  d$set("f", 1)   # This object is small and shouldn't be pruned.
  d$prune()
  expect_identical(d$keys(), "f")
})


test_that("MemoryCache: handling missing values", {
  d <- memoryCache()
  expect_true(is.key_missing(d$get("abcd")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = NULL), NULL)
  expect_error(
    d$get("y", missing = function(key) stop("Missing key: ", key), exec_missing = TRUE),
    "^Missing key: y$",
  )

  d <- memoryCache(missing = NULL)
  expect_true(is.null(d$get("abcd")))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = -1), -1)
  expect_error(
    d$get("y", missing = function(key) stop("Missing key: ", key), exec_missing = TRUE),
    "^Missing key: y$",
  )

  d <- memoryCache(missing = function(key) stop("Missing key: ", key), exec_missing = TRUE)
  expect_error(d$get("abcd"), "^Missing key: abcd$")
  # When exec_missing==TRUE, should be able to set a value that's identical to
  # missing.
  d$set("x", NULL)
  d$set("x", function(key) stop("Missing key: ", key))
  d$set("a", 100)
  expect_identical(d$get("a"), 100)
  expect_identical(d$get("y", missing = NULL, exec_missing = FALSE), NULL)
  expect_true(is.key_missing(d$get("y", missing = key_missing(), exec_missing = FALSE)))
  expect_error(
    d$get("y", missing = function(key) stop("Missing key 2: ", key), exec_missing = TRUE),
    "^Missing key 2: y$",
  )

  # Can't create a cache with both missing and missing_f
  expect_error(memoryCache(missing = 1, exec_missing = TRUE))
})
