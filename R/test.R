
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
#' TODO: take a regex
#' @param appDir The base directory for the application.
#'
#' @details Historically, [shinytest](https://rstudio.github.io/shinytest/)
#'   recommended placing tests at the top-level of the `tests/` directory. In
#'   order to support that model, `testApp` first checks to see if the `.R`
#'   files in the `tests/` directory are all shinytests; if so, just calls out
#'   to [shinytest::testApp()].
#' @export
testApp <- function(appDir="."){
  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)){
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern="\\.r$", ignore.case = TRUE)

  if (length(runners) == 0){
    message("No test runners found in ", testsDir)
    return(structure(list(result=NA, files=list()), class="shinytestrun"))
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

    shinytest::testApp(appDir)
    # TODO: return our result structure
    return()
  }

  # TODO: load supporting R files -- conditioned on the option
  # TODO: shinytest runs in a new process which won't inherit that option^

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  }, add=TRUE)

  setwd(testsDir)

  # Otherwise source all the runners -- each in their own environment.
  fileResults <- list()
  lapply(runners, function(r){
    env <- new.env(parent=emptyenv())
    tryCatch({sourceUTF8(r, envir=env); fileResults[r] <<- TRUE}, error=function(e){
      fileResults[r] <<- FALSE
    })
  })

  return(structure(list(results=all(as.logical(fileResults)), files=fileResults), class="shinytestrun"))
}
