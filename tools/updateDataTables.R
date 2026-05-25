#!/usr/bin/env Rscript

# This script downloads DataTabes from its GitHub repository,
# https://github.com/DataTables/DataTables

# This script can be sourced from RStudio, or run with Rscript.

version <- "1.10.22"
cdn <- file.path("https://cdn.datatables.net", version)

dest <- rprojroot::find_package_root_file("inst/www/shared/datatables")

withr::with_dir(dest, {

  files <- c(
    "js/jquery.dataTables.min.js",
    "js/dataTables.bootstrap.js",
    "css/dataTables.bootstrap.css",
    "images/sort_desc.png",
    "images/sort_desc_disabled.png",
    "images/sort_both.png",
    "images/sort_asc.png",
    "images/sort_asc_disabled.png"
  )

  lapply(files, function(f) {
    download.file(file.path(cdn, f), f)
  })

})

