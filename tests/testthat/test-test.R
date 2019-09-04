
context ("testApp")

test_that("testApp works", {
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

  # Temporarily opt-in to R/ file autoloading
  orig <- getOption("shiny.autoload.r", NULL)
  options(shiny.autoload.r=TRUE)
  on.exit({options(shiny.autoload.r=orig)}, add=TRUE)

  testSpy <- rewire(testApp, sourceUTF8 = sourceStub)

  res <- testSpy(test_path("../test-helpers/app1-standard"))

  # Should have seen two calls to each test runner
  expect_length(calls, 2)
  expect_match(calls[[1]][[1]], "runner1\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "runner2\\.R$", perl=TRUE)

  # Check environments
  # Each should be loaded into an isolated env that's a child of emptyenv
  env1 <- calls[[1]]$envir
  expect_identical(parent.env(env1), emptyenv())

  env2 <- calls[[2]]$envir
  expect_identical(parent.env(env2), emptyenv())

  expect_true(!identical(env1, env2))

  # Check working directory
  expect_equal(normalizePath(wd), normalizePath(
    file.path(test_path("../test-helpers/app1-standard"), "tests")))

  # Check the results
  expect_equal(res$result, FALSE)
  expect_equal(res$files, list(`runner1.R` = TRUE, `runner2.R` = FALSE))
  expect_s3_class(res, "shinytestrun")

  # Clear out err'ing files and rerun
  filesToError <- character(0)

  res <- testSpy(test_path("../test-helpers/app1-standard"))
  expect_equal(res$result, TRUE)
  expect_equal(res$files, list(`runner1.R` = TRUE, `runner2.R` = TRUE))
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
  testSpy <- rewire(testApp, isShinyTest = isShinyTestStub, requireNamespace = requireNamespaceStub)
  expect_error(testSpy(test_path("../test-helpers/app1-standard")), "but shinytest is not installed")

  # All are shinytests and shinytest is installed
  shinytestInstalled <- TRUE
  sares <- list()
  sares[[1]] <- list(name = "test1", pass=TRUE)
  sares[[2]] <- list(name = "test2", pass=FALSE)
  overloadShinyTest <- rewire_namespace_handler("shinytest", "testApp", function(...){ list(results=sares) })
  testSpy <- rewire(testApp, isShinyTest = isShinyTestStub, requireNamespace = requireNamespaceStub, `::` = overloadShinyTest)

  # Run shinytest with a failure
  res2 <- testSpy(test_path("../test-helpers/app1-standard"))
  expect_false(res2$result)
  expect_equal(res2$files, list(test1=TRUE, test2=FALSE))
  expect_s3_class(res2, "shinytestrun")

  # Run shinytest with all passing
  sares[[2]]$pass <- TRUE
  res2 <- testSpy(test_path("../test-helpers/app1-standard"))
  expect_true(res2$result)
  expect_equal(res2$files, list(test1=TRUE, test2=TRUE))
  expect_s3_class(res2, "shinytestrun")

  # Not shinytests
  isShinyTest <- FALSE
  res <- testSpy(test_path("../test-helpers/app1-standard"))
  expect_s3_class(res, "shinytestrun")
})

test_that("testApp handles the absence of tests", {
  expect_error(testApp(test_path("../test-helpers/app2-nested")), "No tests directory found")
  expect_message(res <- testApp(test_path("../test-helpers/app6-empty-tests")), "No test runners found in")
  expect_equal(res$result, NA)
  expect_equal(res$files, list())
  expect_s3_class(res, "shinytestrun")
})
