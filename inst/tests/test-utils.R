context("utils")

test_that("Private randomness works at startup", {

  if (exists(".Random.seed", envir = .GlobalEnv))
    rm(".Random.seed", envir = .GlobalEnv)
  .globals$ownSeed <- NULL
  # Just make sure this doesn't blow up
  createUniqueId(4)
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
