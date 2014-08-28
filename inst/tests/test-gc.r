context("garbage collection")

test_that("unreferenced observers are garbage collected", {
  vals_removed <- FALSE
  obs_removed  <- FALSE
  vals <- reactiveValues(A=1)
  obs  <- observe({ vals$A })

  # These are called when the objects are garbage-collected
  reg.finalizer(.subset2(vals,'impl'), function(e) vals_removed <<- TRUE)
  reg.finalizer(obs, function(e) obs_removed  <<- TRUE)

  flushReact()

  # Removing this reference to obs doesn't delete it because vals still has a
  # reference to it
  rm(obs)
  invisible(gc())
  expect_equal(c(vals_removed, obs_removed), c(FALSE, FALSE))

  # Updating vals$A and flushing won't make obs go away because it creates a new
  # context, and vals$A's context tracks obs's context as a dependent
  vals$A <- 2
  flushReact()
  invisible(gc())
  expect_equal(c(vals_removed, obs_removed), c(FALSE, FALSE))

  # Removing vals will result in vals and obs being garbage collected since
  # there are no other references to them
  rm(vals)
  invisible(gc())
  expect_equal(c(vals_removed, obs_removed), c(TRUE, TRUE))
})


test_that("suspended observers are garbage collected", {
  vals_removed <- FALSE
  obs_removed  <- FALSE
  vals <- reactiveValues(A=1)
  obs  <- observe({ vals$A })

  # These are called when the objects are garbage-collected
  reg.finalizer(.subset2(vals,'impl'), function(e) vals_removed <<- TRUE)
  reg.finalizer(obs, function(e) obs_removed <<- TRUE)

  flushReact()

  vals$A <- 2
  flushReact()
  invisible(gc())

  # Simply suspending and removing our reference to obs doesn't result in GC,
  # because vals's context still has a reference to obs's context, as a dependent
  obs$suspend()
  rm(obs)
  invisible(gc())
  expect_equal(c(vals_removed, obs_removed), c(FALSE, FALSE))

  # Next time we update vals$A and flush, there's no more reference to obs
  vals$A <- 3
  flushReact()
  invisible(gc())
  expect_equal(c(vals_removed, obs_removed), c(FALSE, TRUE))

  # Deleting vals should work immediately now
  rm(vals)
  invisible(gc())  # Removes vals object
  expect_equal(c(vals_removed, obs_removed), c(TRUE, TRUE))
})
