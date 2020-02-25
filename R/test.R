#' Creates and returns run result data frame.
#'
#' @param file Name of the test runner file, a character vector of length 1.
#' @param pass Whether or not the test passed, a logical vector of length 1.
#' @param result Value (wrapped in a list) obtained by evaluating `file` or `NA`
#'   if no value was obtained, such as with `shinytest`.
#' @param error Error, if any, (and wrapped in a list) that was signaled during
#'   evaluation of `file`.
#'
#' @return A 1-row data frame representing a single test run. `result` and
#'   `error` are "list columns", or columns that may contain list elements.
#' @noRd
result_row <- function(file, pass, result, error) {
  stopifnot(is.list(result))
  stopifnot(is.list(error))
  df <- data.frame(
    file = file,
    pass = pass,
    result = I(result),
    error = I(error),
    stringsAsFactors = FALSE
  )
  class(df) <- c("shinytestrun", class(df))
  df
}

#' Check to see if the given text is a shinytest
#' Scans for the magic string of `app <- ShinyDriver$new(` as an indicator that this is a shinytest.
#' Brought in from shinytest to avoid having to export this function.
#' @noRd
isShinyTest <- function(text){
  lines <- grepl("app\\s*<-\\s*ShinyDriver\\$new\\(", text, perl=TRUE)
  any(lines)
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
#'
#' @return A data frame classed with the supplemental class `"shinytestrun"`.
#'   The data frame has the following columns:
#'
#' | **Name** | **Type** | **Meaning** |
#' | :-- | :-- | :-- |
#' | `file` | `character(1)` | File name of the runner script in `tests/` that was sourced. |
#' | `pass` | `logical(1)` | Whether or not the runner script signaled an error when sourced. |
#' | `result` | any or `NA` | The return value of the runner, or `NA` if `pass == FALSE`. |
#' | `error` | any or `NA` | The error signaled by the runner, or `NA` if `pass == TRUE`. |
#'
#' @details Historically, [shinytest](https://rstudio.github.io/shinytest/)
#'   recommended placing tests at the top-level of the `tests/` directory. In
#'   order to support that model, `testApp` first checks to see if the `.R`
#'   files in the `tests/` directory are all shinytests; if so, just calls out
#'   to [shinytest::testApp()].
#' @export
runTests <- function(appDir=".", filter=NULL){
  require(shiny)

  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)){
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern="\\.r$", ignore.case = TRUE)

  if (length(runners) == 0){
    message("No test runners found in ", testsDir)
    return(result_row(character(0), logical(0), list(), list()))
  }

  if (!is.null(filter)){
    runners <- runners[grepl(filter, runners)]
  }
  if (length(runners) == 0){
    stop("No test runners matched the given filter: '", filter, "'")
  }

  # Inspect each runner to see if it appears to be a shinytest
  isST <- vapply(runners, function(r){
    text <- readLines(file.path(testsDir, r), warn = FALSE)
    isShinyTest(text)
  }, logical(1))

  if (all(isST)){
    # just call out to shinytest
    # We don't need to message/warn here since shinytest already does it.
    if (!requireNamespace("shinytest", quietly=TRUE) ){
      stop("It appears that the .R files in ", testsDir,
           " are all shinytests, but shinytest is not installed.")
    }

    if (!getOption("shiny.autoload.r", TRUE)) {
      warning("You've disabled `shiny.autoload.r` via an option but this is not passed through to shinytest. Consider using a _disable_autoload.R file as described at https://rstd.io/shiny-autoload")
    }

    return(do.call(rbind, lapply(shinytest::testApp(appDir)[["results"]], function(r) {
      error <- if (r[["pass"]]) NA else simpleError("Unknown shinytest error")
      result_row(r[["name"]], r[["pass"]], list(NA), list(error))
    })))
  }

  testenv <- new.env(parent=globalenv())
  renv <- new.env(parent=testenv)
  if (getOption("shiny.autoload.r", TRUE)) {
    loadSupport(appDir, renv=renv, globalrenv=testenv)
  } else if (file.exists.ci(file.path(appDir, "server.R"))){
    # then check for global.R to load
    if (file.exists(file.path.ci(appDir, "global.R"))){
      sourceUTF8(file.path.ci(appDir, "global.R"))
    }
  }

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  }, add=TRUE)

  setwd(testsDir)

  # Otherwise source all the runners -- each in their own environment.
  return(do.call(rbind, lapply(runners, function(r) {
    result <- NA
    error <- NA
    pass <- FALSE
    tryCatch({
      env <- new.env(parent = renv)
      result <- sourceUTF8(r, envir = env)
      pass <- TRUE
    }, error = function(e) {
      error <<- e
    })
    result_row(r, pass, list(result), list(error))
  })))
}
