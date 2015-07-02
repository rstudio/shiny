#!/usr/bin/env Rscript

# This script downloads strftime-min.js from its GitHub repository,
# https://github.com/samsonjs/strftime

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

ref <- "v0.9.2"
destdir <- file.path(dirname(thisFile()), "../inst/www/shared/strftime/")

download.file(
  paste0("https://raw.githubusercontent.com/samsonjs/strftime/", ref, "/strftime-min.js"),
  destfile = file.path(destdir, "strftime-min.js")
)
