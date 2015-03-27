#!/usr/bin/env Rscript

# This script copies resources from ion.RangeSlider to shiny's inst
# directory. The ion.RangeSlider/ project directory should be on the same level
# as the shiny/ project directory.

# This script can be sourced from RStudio, or run with Rscript.

# Returns the file currently being sourced or run with Rscript
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

srcdir <- file.path(dirname(thisFile()), "../../ion.rangeSlider")
destdir <- file.path(dirname(thisFile()), "../inst/www/shared/ionrangeslider")

file.copy(file.path(srcdir, "js/ion.rangeSlider.js"), file.path(destdir, "js"),
          overwrite = TRUE)
file.copy(file.path(srcdir, "css"), destdir, recursive = TRUE)
file.copy(file.path(srcdir, "img"), destdir, recursive = TRUE)
