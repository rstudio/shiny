#' Creates and returns run result data frame.
#'
#' @param file Name of the test runner file, a character vector of length 1.
#' @param pass Whether or not the test passed, a logical vector of length 1.
#' @param result Value (wrapped in a list) obtained by evaluating `file`.
#'   This can also by any errors signaled when evaluating the `file`.
#'
#' @return A 1-row data frame representing a single test run. `result` and
#'   is a "list column", or a column that contains list elements.
#' @noRd
result_row <- function(file, pass, result) {
  stopifnot(is.list(result))
  df <- data.frame(
    file = file,
    pass = pass,
    result = I(result),
    stringsAsFactors = FALSE
  )
  class(df) <- c("shiny_runtests", class(df))
  df
}

#' Check to see if the given directory contains at least one script, and that
#' all scripts in the directory are shinytest scripts.
#' Scans for the magic string of `app <- ShinyDriver$new(` as an indicator that
#' this is a shinytest.
#' @noRd
is_legacy_shinytest_dir <- function(path){
  is_shinytest_script <- function(file) {
    if (!file.exists(file)) {
      return(FALSE)
    }

    text <- readLines(file, warn = FALSE)
    any(
      grepl("app\\s*<-\\s*ShinyDriver\\$new\\(", text, perl=TRUE)
    )
  }

  files <- dir(path, full.names = TRUE)
  files <- files[!file.info(files)$isdir]
  if (length(files) == 0) {
    return(FALSE)
  }
  all(vapply(files, is_shinytest_script, logical(1)))
}

#' Runs the tests associated with this Shiny app
#'
#' Sources the `.R` files in the top-level of `tests/` much like `R CMD check`.
#' These files are typically simple runners for tests nested in other
#' directories under `tests/`.
#'
#' @param appDir The base directory for the application.
#' @param filter If not `NULL`, only tests with file names matching this regular
#'   expression will be executed. Matching is performed on the file name
#'   including the extension.
#' @param assert Logical value which determines if an error should be thrown if any error is captured.
#' @param envir Parent testing environment in which to base the individual testing environments.
#'
#' @return A data frame classed with the supplemental class `"shiny_runtests"`.
#'   The data frame has the following columns:
#'
#' | **Name** | **Type** | **Meaning** |
#' | :-- | :-- | :-- |
#' | `file` | `character(1)` | File name of the runner script in `tests/` that was sourced. |
#' | `pass` | `logical(1)` | Whether or not the runner script signaled an error when sourced. |
#' | `result` | any or `NA` | The return value of the runner |
#'
#' @details Historically, [shinytest](https://rstudio.github.io/shinytest/)
#'   recommended placing tests at the top-level of the `tests/` directory.
#'   This older folder structure is not supported by runTests.
#'   Please see [shinyAppTemplate()] for more details.
#' @export
runTests <- function(
  appDir = ".",
  filter = NULL,
  assert = TRUE,
  envir = globalenv()
) {
  # similar to runApp()
  # Allows shiny's functions to be available in the UI, server, and test code
  require(shiny)

  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)) {
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern="\\.r$", ignore.case = TRUE)

  if (length(runners) == 0) {
    message("No test runners found in ", testsDir)
    return(result_row(character(0), logical(0), list()))
  }

  if (!is.null(filter)) {
    runners <- runners[grepl(filter, runners)]
  }
  if (length(runners) == 0) {
    stop("No test runners matched the given filter: '", filter, "'")
  }

  # See the @details section of the runTests() docs above for why this branch exists.
  if (is_legacy_shinytest_dir(testsDir)) {
    stop(
      "It appears that the .R files in ", testsDir, " are all shinytests.",
      " This is not supported by `shiny::runTests()`.",
      "\nPlease see `?shiny::migrateLegacyShinytest` to migrate your shinytest file structure to the new format.",
      "\nSee `?shiny::shinyAppTemplate` for an example of the new testing file structure."
    )
  }

  renv <- new.env(parent = envir)

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  }, add = TRUE)
  setwd(testsDir)

  # Otherwise source all the runners -- each in their own environment.
  ret <- do.call(rbind, lapply(runners, function(r) {
    pass <- FALSE
    result <-
      tryCatch({
        env <- new.env(parent = renv)
        ret <- sourceUTF8(r, envir = env)
        pass <- TRUE
        ret
      }, error = function(err) {
        message("Error in ", r, "\n", err)
        err
      })
    result_row(file.path(testsDir, r), pass, list(result))
  }))

  if (isTRUE(assert)) {
    if (!all(ret$pass)) {
      stop("Shiny App Test Failures detected in\n", paste0("* ", runtest_pretty_file(ret$file[!ret$pass]), collapse = "\n"), call. = FALSE)
    }
  }

  ret
}


runtest_pretty_file <- function(f) {
  test_folder <- dirname(f)
  app_folder <- dirname(test_folder)
  file.path(
    basename(app_folder),
    basename(test_folder),
    basename(f)
  )
}


print.shiny_runtests <- function(x, ..., reporter = "summary") {

  cat("Shiny App Test Results\n")


  if (any(x$pass)) {
    # TODO in future... use clisymbols::symbol$tick and crayon green
    cat("* Success\n")
    mapply(
      x$file,
      x$pass,
      x$result,
      FUN = function(file, pass, result) {
        if (!pass) return()
        # print(result)
        cat("  - ", runtest_pretty_file(file), "\n", sep = "")
      }
    )
  }
  if (any(!x$pass)) {
    # TODO in future... use clisymbols::symbol$cross and crayon red
    cat("* Failure\n")
    mapply(
      x$file,
      x$pass,
      x$result,
      FUN = function(file, pass, result) {
        if (pass) return()
        cat("  - ", runtest_pretty_file(file), "\n", sep = "")
      }
    )
  }

  invisible(x)
}


#' Migrate legacy \pkg{shinytest} files to new test directory structure
#'
#' This function migrates the old-style directory structure used by
#' \pkg{shinytest} to (versions 1.3.1 and below) new test directory structure
#' used in Shiny 1.5.0 and above.
#'
#' In Shiny 1.5.0, the [runTests()] function was added, and it will run test
#' scripts tests/ subdirectory of the application. The directory structure will
#' look something like this:
#'
#' ```
#' appdir/
#'  |- R
#'  |- tests
#'      |- shinytest.R
#'      |- shinytest
#'      |   `- mytest.R
#'      |- testthat.R
#'      `- testthat
#'          `- test-script.R
#' ```
#'
#' This allows for tests using the \pkg{shinytest} package as well as other
#' testing tools, such as the [testServer()] function, which can be used for
#' testing module and server logic, and for unit tests of functions in an R/
#' subdirectory.
#'
#'
#' With the \pkg{shinytest} package, in versions 1.3.0 and below, the tests/
#' subdirectory of the application was used specifically for \pkg{shinytest},
#' and could not be used for other types of tests. So the directory structure
#' would look like this:
#'
#' ```
#' appdir/
#'  `- tests
#'      `- mytest.R
#' ```
#'
#' In \pkg{shinytest} 1.4.0 and above, it defaults to the new directory
#' structure.
#'
#' @param appdir A directory containing a Shiny application.
#' @param prompt If \code{TRUE}, ask for confirmation when moving files.
#'
#' @export
migrateLegacyShinytest <- function(appdir, prompt = interactive()) {
  appdir <- findEnclosingApp(appdir)

  test_dir <- file.path(appdir, "tests")
  shinytest_dir <- file.path(test_dir, "shinytest")

  if (!is_legacy_shinytest_dir(test_dir)) {
    stop("The .R files in ", test_dir,
         " must all be test scripts for the shinytest package.")
  }

  if (prompt) {
    res <- readline(
      paste0(
        "Creating ", shinytest_dir, " and moving files from\n", test_dir,
        " into it.\nAre you sure you want to do this? [y/n]\n"
      )
    )
    if (!identical(tolower(res), "y")) {
      return(invisible())
    }
  }

  files <- dir(test_dir)
  dir.create(shinytest_dir, showWarnings = FALSE)
  file.rename(file.path(test_dir, files), file.path(shinytest_dir, files))
  invisible()
}
