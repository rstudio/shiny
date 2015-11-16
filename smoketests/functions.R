appdirs <- function() {
  res <- list.dirs(full.names = FALSE, recursive = FALSE)
  res[res != "output"]
}

executeApp <- function(appPath) {
  if (system2("which", "phantomjs", stdout = NULL) != 0) {
    stop("phantomjs must be installed and on the system path")
  }

  system2("phantomjs", "visit.js", wait = FALSE)
  result <- system2(
  	"R",
	c("--slave", "-e",
      shQuote(sprintf("shiny::runApp('%s', port = 8765)", appPath))
	),
    stdout = TRUE, stderr = TRUE
  )
  gsub(getwd(), "${PWD}", result)
}

# Returns TRUE if the files indicated in artifactPaths exist, and
# have mtimes that are later than or equal to all the other mtimes
# in the dir.
# Example:
# upToDate("./bin", "a.out")
upToDate <- function(dirname, artifactPaths) {
  files <- list.files(dirname, recursive = TRUE)
  if (!all(artifactPaths %in% files)) {
    # One or more artifacts missing
    return(FALSE)
  }
  times <- file.mtime(file.path(dirname, files))
  artifactTimes <- times[files %in% artifactPaths]

  max(artifactTimes) >= max(times)
}
