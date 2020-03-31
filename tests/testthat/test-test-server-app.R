context("testServer app")

library(shiny)
library(testthat)

test_that("testServer works with dir app", {
  # app.R
  testServer(test_path("..", "test-modules", "06_tabsets"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })

  # server.R
  testServer(test_path("..", "test-modules", "server_r"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })
})
#
#test_that("testServer works when referencing external globals", {
#  # If global is defined at the top of app.R outside of the server function.
#  testServer({
#    expect_equal(get("global", session$env), 123)
#  }, appDir=test_path("..", "test-modules", "06_tabsets"))
#})
#
#test_that("findApp errors with no app", {
#  calls <- 0
#  nothingExists <- function(path){
#    calls <<- calls + 1
#    FALSE
#  }
#  fa <- rewire(findApp, file.exists.ci=nothingExists)
#  expect_error(
#    expect_warning(fa("/some/path/here"), "No such file or directory"), # since we just made up a path
#    "No shiny app was found in ")
#  expect_equal(calls, 4 * 2) # Checks here, path, some, and / -- looking for app.R and server.R for each
#})

#test_that("findApp works with app in current or parent dir", {
#  calls <- 0
#  cd <- normalizePath(".")
#  mockExists <- function(path){
#    # Only TRUE if looking for server.R or app.R in current Dir
#    calls <<- calls + 1

#    path <- normalizePath(path, mustWork = FALSE)

#    appPath <- normalizePath(file.path(cd, "app.R"), mustWork = FALSE)
#    serverPath <- normalizePath(file.path(cd, "server.R"), mustWork = FALSE)
#    return(path %in% c(appPath, serverPath))
#  }
#  fa <- rewire(findApp, file.exists.ci=mockExists)
#  expect_equal(fa(), cd)
#  expect_equal(calls, 1) # Should get a hit on the first call and stop

#  # Reset and point to the parent dir
#  calls <- 0
#  cd <- normalizePath("..") # TODO: won't work if running tests in the root dir.
#  f <- fa()
#  expect_equal(normalizePath(f, mustWork = FALSE), cd)
#  expect_equal(calls, 3) # Two for current dir and hit on the first in the parent
#})
