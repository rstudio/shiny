context("utils")

test_that("Private randomness works at startup", {

  if (exists(".Random.seed", envir = .GlobalEnv))
    rm(".Random.seed", envir = .GlobalEnv)
  .globals$ownSeed <- NULL
  # Just make sure this doesn't blow up
  expect_error(createUniqueId(4), NA)
})

test_that("Setting process-wide seed doesn't affect private randomness", {
  set.seed(0)
  id1 <- createUniqueId(4)
  set.seed(0)
  id2 <- createUniqueId(4)

  expect_false(identical(id1, id2))
})

test_that("Resetting private seed doesn't result in dupes", {
  .globals$ownSeed <- NULL
  id3 <- createUniqueId(4)
  # Make sure we let enough time pass that reinitializing the seed is
  # going to result in a different value. This is especially required
  # on Windows.
  Sys.sleep(1)
  set.seed(0)
  .globals$ownSeed <- NULL
  id4 <- createUniqueId(4)

  expect_false(identical(id3, id4))
})

test_that("Clearing process-wide seed doesn't affect private randomness", {
  set.seed(NULL)
  id5 <- createUniqueId(4)
  set.seed(NULL)
  id6 <- createUniqueId(4)

  expect_false(identical(id5, id6))
})

test_that("Setting the private seed explicitly results in identical values", {
  set.seed(0)
  .globals$ownSeed <- .Random.seed
  id7 <- createUniqueId(4)
  set.seed(0)
  .globals$ownSeed <- .Random.seed
  id8 <- createUniqueId(4)

  expect_identical(id7, id8)
})

test_that("Private and 'public' random streams are independent and work the same", {
  set.seed(0)
  public <- c(runif(1), runif(1), runif(1))
  withPrivateSeed(set.seed(0))
  private <- c(withPrivateSeed(runif(1)), withPrivateSeed(runif(1)), withPrivateSeed(runif(1)))
  expect_identical(public, private)

  # Interleaved calls to runif() with private and public streams
  set.seed(0)
  withPrivateSeed(set.seed(0))
  public  <- numeric()
  private <- numeric()
  public[1]  <- runif(1)
  private[1] <- withPrivateSeed(runif(1))
  private[2] <- withPrivateSeed(runif(1))
  public[2]  <- runif(1)
  public[3]  <- runif(1)
  private[3] <- withPrivateSeed(runif(1))
  expect_identical(public, private)
})

test_that("need() works as expected", {

  # These are all falsy

  expect_false(need(FALSE, FALSE))
  expect_false(need(NULL, FALSE))
  expect_false(need("", FALSE))

  expect_false(need(character(0), FALSE))
  expect_false(need(logical(0), FALSE))
  expect_false(need(numeric(0), FALSE))
  expect_false(need(integer(0), FALSE))
  expect_false(need(complex(0), FALSE))
  expect_false(need(matrix(), FALSE))

  expect_false(need(NA, FALSE))
  expect_false(need(NA_integer_, FALSE))
  expect_false(need(NA_real_, FALSE))
  expect_false(need(NA_complex_, FALSE))
  expect_false(need(NA_character_, FALSE))

  expect_false(need(c(NA, NA, FALSE), FALSE))
  expect_false(need(c(FALSE), FALSE))

  expect_false(need(try(stop("boom"), silent = TRUE), FALSE))

  # These are all truthy

  expect_null(need(0, FALSE))
  expect_null(need(1:10, FALSE))
  expect_null(need(LETTERS, FALSE))
  expect_null(need("NA", FALSE))
  expect_null(need(TRUE, FALSE))
  expect_null(need(c(NA, NA, TRUE), FALSE))
  expect_null(need(c(FALSE, FALSE, TRUE), FALSE))
})

test_that("req works", {
  expect_error(req(TRUE, FALSE))
  expect_error(req(TRUE, stop("boom")))
  expect_equivalent(req(1, TRUE), 1)

  # req arguments short circuit when a falsy value is found
  value <- 0
  expect_error(req(NULL, value <- 1))
  expect_equal(value, 0)

  # first argument is evaluated exactly once
  value <- 0
  req(value <- value + 1)
  expect_equal(value, 1)

  # correct environment is used
  req2 <- function(...) {
    req(...)
  }
  value <- 0
  req2(value <- value + 1)
  expect_equal(value, 1)

  # Lots of args are supported
  expect_error(do.call(req, c(as.list(1:1000), list(NULL))))
  expect_equal(1, do.call(req, as.list(1:1000)))
})

test_that("anyUnnamed works as expected", {
  expect_false(anyUnnamed(list()))
  expect_true(anyUnnamed(list(1,2,3)))
  expect_true(anyUnnamed(list(A = 1,2,3)))
  expect_false(anyUnnamed(list(A = 1,B = 2,C = 3)))

  # List with named elements removed
  x <- list(A = 1, B = 2, 3, 4)
  x <- x[3:4]
  expect_true(anyUnnamed(x))
})

test_that("sortByName works as expected", {
  # Error if any unnamed elements
  expect_error(sortByName(c("a", "b")))
  expect_error(sortByName(list(a=1, 2)))

  expect_identical(sortByName(NULL), NULL)
  expect_identical(sortByName(numeric(0)), numeric(0))
  expect_identical(sortByName(character(0)), character(0))
  # Empty unnamed list
  expect_identical(sortByName(list()), list())
  # Empty named list
  expect_identical(sortByName(list(a=1)[0]), list(a=1)[0])

  expect_identical(sortByName(list(b=1, a=2)), list(a=2, b=1))
  expect_identical(sortByName(list(b=1)), list(b=1))

  # Ties are resolved by using original order
  expect_identical(sortByName(list(b=1, a=2, b=3)), list(a=2, b=1, b=3))
  expect_identical(sortByName(list(b=3, a=2, b=1)), list(a=2, b=3, b=1))

  # Make sure atomic vectors work
  expect_identical(sortByName(c(b=1, a=2)), c(a=2, b=1))
  expect_identical(sortByName(c(b=1, a=2, b=3)), c(a=2, b=1, b=3))
})

test_that("Callbacks fire in predictable order", {
  cb <- Callbacks$new()

  x <- numeric(0)
  cb$register(function() {
    x <<- c(x, 1)
  })
  cb$register(function() {
    x <<- c(x, 2)
  })
  cb$register(function() {
    x <<- c(x, 3)
  })
  cb$invoke()
  expect_equal(x, c(1, 2, 3))
})
