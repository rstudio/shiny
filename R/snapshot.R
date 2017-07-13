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
snapshotPreprocessOutput <- function(x, fun) {
  markOutputAttrs(x, snapshotPreprocess = fun)
}


#' Add a function for preprocessing an input before taking a test snapshot
#'
#' @param inputId Name of the input value.
#' @param fun A function that takes the input value and returns a modified
#'   value. The returned value will be used for the test snapshot.
#' @param session A Shiny session object.
#'
#' @export
snapshotPreprocessInput <- function(inputId, fun, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("snapshotPreprocessInput() needs a session object.")
  }

  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.snapshot.preprocess", fun)
}


# Strip out file path from fileInput value
snapshotPreprocessorFileInput <- function(value) {
  value$datapath <- basename(value$datapath)
  value
}
