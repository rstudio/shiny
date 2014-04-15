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
