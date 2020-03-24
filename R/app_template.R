#' Generate a Shiny application template
#'
#' This function populates a directory with files for a Shiny application. They
#' are based off of the "12_counter" example which can be run with
#' `runExample()`.
#'
#' The example application includes the following files and directories:
#'
#' ```
#' appdir/
#' ├── app.R
#' ├── R
#' │   └── counter-module.R
#' └── tests
#'     ├── integration.R
#'     ├── integration
#'     │   └── test-counter.R
#'     ├── shinytest.R
#'     ├── shinytests
#'     │   └── mytest.R
#'     ├── testthat.R
#'     └── testthat
#'         ├── helper-load.R
#'         └── test-counter.R
#' ```
#'
#' Some notes about these files:
#' * app.R is the main application file.
#' * All files in the R/ subdirectory are automatically sourced when the
#'   application is run.
#' * The R/counter-module.R file is automatically sourced when the application
#'   is run. This file contains code for a [Shiny module](moduleServer()) which
#'   is used in the application.
#' * The tests/ directory contains various tests for the application. You may
#'   choose to use or remove any of them. They can be executed by the
#'   [runTests()] function.
#' * tests/integration.R is a test runner for test files in the
#'   tests/integration/ directory.
#' * tests/integration/test-counter.R is a test for the counter module.
#' * tests/shinytest.R is a test runner for test files in the
#'   tests/shinytest/ directory.
#' * tests/shinytest/mytest.R is a test that uses the
#'   [shinytest](https://rstudio.github.io/shinytest/) package to do
#'   snapshot-based testing.
#' * tests/testthat.R is a test runner for test files in the
#'   tests/testthat/ directory.
#' * tests/testthat/test-counter.R is a set of tests that use the
#'   [testthat](https://testthat.r-lib.org/) package for testing.
#' * tests/testthat/helper-load.R is a helper script that is automatically
#'   loaded before running test-counter.R. (This is performed by the testthat
#'   package.)
#'
#' @param path Path to create new shiny application template.
#' @param examples Should the new path include example code? Defaults to `TRUE`.
#' @param quiet Should status information be printed? Defaults to `FALSE`.
#' @param interactive If `TRUE`, the user may be asked questions and prompted
#'   for a response.
#'
#' @export
shinyAppTemplate <- function(path = NULL, ..., examples = TRUE, quiet = FALSE,
  interactive = base::interactive())
{
  # Check if a directory is empty, ignoring certain files
  dir_is_empty <- function(path) {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    # Ignore .DS_Store files, which are sometimes automatically created on macOS
    files <- setdiff(files, ".DS_Store")
    return(length(files) != 0)
  }

  # Little helper to resolve paths relative to our example
  example_path <- function(path) {
    system.file("examples", "12_counter", path, package = "shiny")
  }

  if (is.null(path)) {
    stop("`path` is missing.")
  }
  if (file.exists(path) && !dir.exists(path)) {
    stop(path, " exists but is not a directory.")
  }

  if (dir.exists(path) && dir_is_empty(path)) {
    if (interactive) {
      response <- readline(paste0(
        ensure_trailing_slash(path),
        " is not empty. Do you want to create a Shiny app in this directory anyway? [y/n] "
      ))
      if (tolower(response) != "y") {
        return(invisible())
      }
    }
  } else {
    dir.create(path)
  }

  # The R/ dir
  r_dir <- file.path(path, "R")
  dir.create(r_dir)
  if (examples) {
    file.copy(example_path("R/counter.R"), r_dir)
  }

  # The tests/ dir
  tests_dir <- file.path(path, "tests")
  dir.create(tests_dir)
  file.copy(
    example_path(c("tests/shinytests.R", "tests/testthat.R", "tests/integration.R")),
    tests_dir
  )
  if (!examples) {
    dir.create(file.path(tests_dir, "shinytests"))
    dir.create(file.path(tests_dir, "testthat"))
    dir.create(file.path(tests_dir, "integration"))
  } else {
    file.copy(example_path("tests/shinytests"), tests_dir, recursive = TRUE)
    file.copy(example_path("tests/testthat"),   tests_dir, recursive = TRUE)
    file.copy(example_path("tests/integration"),tests_dir, recursive = TRUE)
  }

  # app/ui/server.R files
  # Create app.R
  if (!examples) {
    file.create(file.path(path, "app.R"))
  } else {
    file.copy(example_path("app.R"), path)
  }

  if (!quiet) {
    message("Shiny app template created at ", ensure_trailing_slash(path))
  }
}
