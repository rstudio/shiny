
context("app")

test_that("helpers are loaded into the right env", {
  env <- new.env(parent=environment())
  loadHelpers("../test-helpers/app1-standard", envir=env)
  expect_equal(get("helper1", env), 123)
  expect_equal(get("helper2", env), "abc")
})

test_that("nested helpers are loaded", {
  loadHelpers("../test-helpers/app2-nested")
  expect_equal(helper1, 456)
  expect_false(exists("helper2"))
})

test_that("app with both r/ and R/ prefers R/", {
  ## App 4 already has a lower-case r/ directory. Try to create an upper.
  tryCatch(dir.create("../test-helpers/app4-both/R"),
           warning=function(w){testthat::skip("File system is not case-sensitive")})
  writeLines("upperHelper <- 'abc'", file.path("../test-helpers/app4-both/R", "upper.R"))

  loadHelpers("../test-helpers/app4-both")

  expect_false(exists("lowerHelper"))
  expect_equal(upperHelper, "abc")
})

test_that("With ui/server.R, global.R is loaded before R/ helpers and into the right envs", {
  calls <- list()
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    NULL
  }

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadHelpers
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadHelpers, sourceUTF8 = sourceStub)
  sad <- rewire(shinyAppDir_serverR, sourceUTF8 = sourceStub, loadHelpers = loadSpy)

  sa <- sad(normalizePath("../test-helpers/app1-standard"))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen three calls -- first to global then to the helpers
  expect_length(calls, 3)
  expect_match(calls[[1]][[1]], "/global\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "/helperCap\\.R$", perl=TRUE)
  expect_match(calls[[3]][[1]], "/helperLower\\.r$", perl=TRUE)

  # Check environments
  # global.R has no env specified -- loaded into the global env.
  expect_length(calls[[1]], 1)
  # helpers are loaded into a child of the global env
  helperEnv1 <- calls[[2]]$envir
  helperEnv2 <- calls[[3]]$envir
  expect_identical(helperEnv1, helperEnv2)
  expect_identical(parent.env(helperEnv1), globalenv())

  calls <- NULL
  # Source the server
  sa$serverFuncSource()
  expect_length(calls, 1)
  # server.R is sourced into a child environment of the helpers
  expect_match(calls[[1]][[1]], "/server\\.R$")
  expect_identical(parent.env(calls[[1]]$envir), helperEnv1)

  calls <- NULL
  # Invoke the UI by simulating a request
  sa$httpHandler(list())
  expect_length(calls, 1)
  # ui.R is sourced into a child environment of the helpers
  expect_match(calls[[1]][[1]], "/ui\\.R$")
  expect_identical(parent.env(calls[[1]]$envir), helperEnv1)
})


test_that("app.R is loaded after R/ helpers and into the right envs", {
  calls <- list()
  sourceSpy <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    do.call(sourceUTF8, list(...))
  }

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadHelpers
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadHelpers, sourceUTF8 = sourceSpy)
  sad <- rewire(shinyAppDir_appR, sourceUTF8 = sourceSpy, loadHelpers = loadSpy)

  sa <- sad("app.R", normalizePath("../test-helpers/app2-nested"))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen three calls -- first to two helpers then to app.R
  expect_length(calls, 2)
  expect_match(calls[[1]][[1]], "/helper\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "/app\\.R$", perl=TRUE)

  # Check environments
  # helpers are loaded into a child of the global env
  helperEnv1 <- calls[[1]]$envir
  expect_identical(parent.env(helperEnv1), globalenv())

  # app.R is sourced into a child environment of the helpers
  expect_identical(parent.env(calls[[2]]$envir), helperEnv1)
})
