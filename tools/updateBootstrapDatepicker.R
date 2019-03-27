#!/usr/bin/env Rscript
# Retrieves a particular version of bootstrap-datepicker:
#  https://github.com/uxsolutions/bootstrap-datepicker
# After retrieving, you can apply patches stored in
# tools/datepicker-patches with applyDatepickerPatches.R

library(rprojroot)

version   <- "1.6.4"
dest_dir  <- rprojroot::find_package_root_file("inst/www/shared/datepicker")
tag       <- paste0("v", version)
dest_file <- file.path(tempdir(), paste0("bootstrap-datepicker-", version, ".zip"))
url       <- sprintf("https://github.com/uxsolutions/bootstrap-datepicker/releases/download/%s/bootstrap-datepicker-%s-dist.zip", tag, version)

download.file(url, dest_file)
unzip(
  dest_file,
  files = c(
    "js/bootstrap-datepicker.js",
    "css/bootstrap-datepicker3.css",
    "css/bootstrap-datepicker3.min.css"
  ),
  exdir = dest_dir
)
