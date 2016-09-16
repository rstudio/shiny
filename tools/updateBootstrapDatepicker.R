#!/usr/bin/env Rscript

# This script copies resources from Bootstrap Datepicker to shiny's inst
# directory. The bootstrap-datepicker/ project directory should be on the same
# level as the shiny/ project directory.

# It is necessary to run Grunt after running this script: This copies the
# un-minified JS file over, and running Grunt minifies it and inlines the locale
# files into the minified JS.

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

srcdir <- normalizePath(file.path(dirname(thisFile()), "../../bootstrap-datepicker/dist"))
destdir <- normalizePath(file.path(dirname(thisFile()), "../inst/www/shared/datepicker"))

file.copy(
  file.path(srcdir, "js", "bootstrap-datepicker.js"),
  file.path(destdir, "js"),
  overwrite = TRUE
)

file.copy(
  dir(file.path(srcdir, "locales"), "\\.js$", full.names = TRUE),
  file.path(destdir, "js", "locales"),
  overwrite = TRUE
)

file.copy(
  dir(file.path(srcdir, "css"), "^bootstrap-datepicker3(\\.min)?\\.css$",
      full.names = TRUE),
  file.path(destdir, "css"),
  overwrite = TRUE
)
