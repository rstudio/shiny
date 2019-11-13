
context ("runTests")

test_that("runTests works", {
  calls <- list()
  # Tracks the working directory we were in as of the last call
  wd <- NULL

  # A collection of file names that should throw an error when sourced
  filesToError <- "runner2.R"

  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    wd <<- getwd()

    if(list(...)[[1]] %in% filesToError){
      stop("I was told to throw an error")
    }

    NULL
  }

  loadCalls <- list()
  loadSupportStub <- function(...){
    loadCalls[[length(calls)+1]] <<- list(...)
    NULL
  }

  # Temporarily opt-in to R/ file autoloading
  orig <- getOption("shiny.autoload.r", NULL)
  options(shiny.autoload.r=TRUE)
  on.exit({options(shiny.autoload.r=orig)}, add=TRUE)

  runTestsSpy <- rewire(runTests, sourceUTF8 = sourceStub, loadSupport=loadSupportStub)

  res <- runTestsSpy(test_path("../test-helpers/app1-standard"))

  # Should have seen two calls to each test runner
  expect_length(calls, 2)
  expect_match(calls[[1]][[1]], "runner1\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "runner2\\.R$", perl=TRUE)

  # Check environments
  # Each should be loaded into an isolated env that has a common parent
  env1 <- calls[[1]]$envir
  env2 <- calls[[2]]$envir
  expect_identical(parent.env(env1), parent.env(env2))
  expect_true(!identical(env1, env2))

  # Check working directory
  expect_equal(normalizePath(wd), normalizePath(
    file.path(test_path("../test-helpers/app1-standard"), "tests")))

  # Check the results
  expect_equal(res$result, FALSE)
  expect_length(res$files, 2)
  expect_equal(res$files[1], list(`runner1.R` = NA_character_))
  expect_equal(res$files[[2]]$message, "I was told to throw an error")
  expect_s3_class(res, "shinytestrun")

  # Check that supporting files were loaded
  expect_length(loadCalls, 1)
  # global should be a child of emptyenv
  ge <- loadCalls[[1]]$globalrenv
  expect_identical(parent.env(ge), globalenv())
  # renv should be a child of our globalrenv
  expect_identical(parent.env(loadCalls[[1]]$renv), ge)

  # Clear out err'ing files and rerun
  filesToError <- character(0)

  res <- runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_equal(res$result, TRUE)
  expect_equal(res$files, list(`runner1.R` = NA_character_, `runner2.R` = NA_character_))

  # If autoload is false, it should still load global.R. Because this load happens in the top-level of the function,
  # our spy will catch it.
  calls <- list()

  # Temporarily opt-out of R/ file autoloading
  orig <- getOption("shiny.autoload.r", NULL)
  options(shiny.autoload.r=FALSE)
  on.exit({options(shiny.autoload.r=orig)}, add=TRUE)

  res <- runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_length(calls, 3)
  expect_match(calls[[1]][[1]], "/global\\.R", perl=TRUE)
})

test_that("calls out to shinytest when appropriate", {
  isShinyTest <- TRUE
  isShinyTestStub <- function(...){
    isShinyTest
  }

  shinytestInstalled <- FALSE
  requireNamespaceStub <- function(...){
    shinytestInstalled
  }

  # All are shinytests but shinytest isn't installed
  runTestsSpy <- rewire(runTests,
                    isShinyTest = isShinyTestStub,
                    requireNamespace = requireNamespaceStub)
  expect_error(runTestsSpy(test_path("../test-helpers/app1-standard")), "but shinytest is not installed")

  # All are shinytests and shinytest is installed
  shinytestInstalled <- TRUE
  sares <- list()
  sares[[1]] <- list(name = "test1", pass=TRUE)
  sares[[2]] <- list(name = "test2", pass=FALSE)
  overloadShinyTest <- rewire_namespace_handler("shinytest", "testApp",
                                                function(...){ list(results=sares) })
  runTestsSpy <- rewire(runTests, isShinyTest = isShinyTestStub, requireNamespace = requireNamespaceStub, `::` = overloadShinyTest)

  # Run shinytest with a failure
  res2 <- runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_false(res2$result)
  expect_equal(res2$files, list(test1=NA_character_, test2=simpleError("Unknown shinytest error")))
  expect_s3_class(res2, "shinytestrun")

  # Run shinytest with all passing
  sares[[2]]$pass <- TRUE
  res2 <- runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_true(res2$result)
  expect_equal(res2$files, list(test1=NA_character_, test2=NA_character_))
  expect_s3_class(res2, "shinytestrun")

  # Not shinytests
  isShinyTest <- FALSE
  res <- runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_s3_class(res, "shinytestrun")
})

test_that("runTests filters", {
  calls <- list()
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    NULL
  }

  runTestsSpy <- rewire(runTests, sourceUTF8 = sourceStub)

  # No filter should see two tests (global.R is sourced from another function)
  runTestsSpy(test_path("../test-helpers/app1-standard"))
  expect_length(calls, 2)

  # Filter down to one (global.R is sourced from another function)
  calls <- list()
  runTestsSpy(test_path("../test-helpers/app1-standard"), filter="runner1")
  expect_length(calls, 1)

  calls <- list()
  expect_error(runTestsSpy(test_path("../test-helpers/app1-standard"), filter="i don't exist"), "matched the given filter")
})

test_that("runTests handles the absence of tests", {
  expect_error(runTests(test_path("../test-helpers/app2-nested")), "No tests directory found")
  expect_message(res <- runTests(test_path("../test-helpers/app6-empty-tests")), "No test runners found in")
  expect_equal(res$result, NA)
  expect_equal(res$files, list())
  expect_s3_class(res, "shinytestrun")
})
