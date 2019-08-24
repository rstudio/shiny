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
unzipped <- tempdir()
unzip(dest_file, exdir = unzipped)

unlink(dest_dir, recursive = TRUE)

dir.create(file.path(dest_dir, "js"), recursive = TRUE)
file.copy(
  file.path(unzipped, "js", "bootstrap-datepicker.js"),
  file.path(dest_dir, "js"),
  overwrite = TRUE
)

dir.create(file.path(dest_dir, "js", "locales"), recursive = TRUE)
file.copy(
  dir(file.path(unzipped, "locales"), "\\.js$", full.names = TRUE),
  file.path(dest_dir, "js", "locales"),
  overwrite = TRUE
)

dir.create(file.path(dest_dir, "css"), recursive = TRUE)
file.copy(
  dir(file.path(unzipped, "css"), "^bootstrap-datepicker3(\\.min)?\\.css$",
      full.names = TRUE),
  file.path(dest_dir, "css"),
  overwrite = TRUE
)
