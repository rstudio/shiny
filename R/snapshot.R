#' Mark an output to be excluded from test snapshots
#'
#' @param x A reactive which will be assigned to an output.
#'
#' @export
snapshotExclude <- function(x) {
  markOutputAttrs(x, snapshotExclude = TRUE)
}

#' Add a function for preprocessing an output before taking a test snapshot
#'
#' @param x A reactive which will be assigned to an output.
#' @param fun A function that takes the output value as an input and returns a
#'   modified value. The returned value will be used for the test snapshot.
#'
#' @export
snapshotPreprocess <- function(x, fun) {
  markOutputAttrs(x, snapshotPreprocess = fun)
}


# fileInputs need preprocessing before being snapshotted. This gets added as a
# meta attribute to the input value. (Note that this is an input, not an
# output, so it's not set via the snapshotPreprocess() function.)
snapshotPreprocessorFileInput <- function(value) {
  value$datapath <- basename(value$datapath)
  value
}
