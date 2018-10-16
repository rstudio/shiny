#!/usr/bin/env Rscript

# This script copies resources from Bootstrap Datepicker to shiny's inst
# directory. It can be run from the command line or sourced from an R session.

version <- "1.8.0"

# The git tag for the https://github.com/uxsolutions/bootstrap-datepicker repo.
tag <- paste0("v", version)

source_file <- file.path(tempdir(), paste0("bootstrap-datepicker-", version, ".tar.gz"))

url <- paste0("https://github.com/uxsolutions/bootstrap-datepicker/archive/", tag, ".tar.gz")
download.file(url, source_file)

untar(source_file, exdir = tempdir())
source_dir <- file.path(tempdir(), paste0("bootstrap-datepicker-", version), "dist")

dest_dir <- rprojroot::find_package_root_file("inst", "www", "shared", "datepicker")
unlink(dest_dir, recursive = TRUE)

dir.create(dest_dir)

locale_files <- dir(file.path(source_dir, "locales"), "\\.min\\.js$")
# Remove errant file from the list of things to copy
locale_files <- locale_files[locale_files != "bootstrap-datepicker-en-CA.min.js"]

filenames <- c(
  "js/bootstrap-datepicker.js",
  # Don't copy over the min.js file, because we minify it, and in the process
  # add in the locale files.
  "css/bootstrap-datepicker3.css",
  "css/bootstrap-datepicker3.min.css",
  file.path("locales", locale_files)
)

copy_files <- function(srcdir, destdir, filenames) {
  # Create needed directories
  dest_subdirs <- file.path(destdir, unique(dirname(filenames)))
  for (dest_subdir in dest_subdirs) {
    dir.create(dest_subdir, recursive = TRUE)
  }

  res <- file.copy(
    from = paste0(srcdir,  "/", filenames),
    to   = paste0(destdir, "/", filenames)
  )

  if (!all(res)) {
    message("Problem copying ", sum(!res), " files: \n  ",
      paste0(filenames[!res], collapse = "\n  ")
    )
  }
}

copy_files(source_dir, dest_dir, filenames)

locales <- sub("bootstrap-datepicker\\.(.*)\\.min\\.js", '"\\1"', locale_files)

message("Make sure to:\n",
  "- Update the version to ", version, " in datePickerDependency.\n",
  "- Run grunt to generate bootstrap-datepicker.min.js.\n",
  "- Update valid languages in documentation for dateInput:\n  ",
  paste0(locales, collapse = ", ")
)
