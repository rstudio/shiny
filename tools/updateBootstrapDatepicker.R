#!/usr/bin/env Rscript
# Retrieves a particular version of bootstrap-datepicker:
#  https://github.com/uxsolutions/bootstrap-datepicker
# After retrieving, applies the series of patches contained
# in tools/datepicker-patches to inst/www/shared/datepicker/

library(rprojroot)

version   <- "1.6.4"
patch_dir <- rprojroot::find_package_root_file("tools/datepicker-patches")
dest_dir  <- rprojroot::find_package_root_file("inst/www/shared/datepicker")

fetch_datepicker <- function() {
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
}

apply_patches <- function() {
}

fetch_datepicker()
apply_patches()
