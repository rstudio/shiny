#!/usr/bin/env Rscript
# Retrieves a particular version of selectize:
#  https://github.com/selectize/selectize.js
# and selectize-plugin-a11y:
#  https://github.com/SLMNBJ/selectize-plugin-a11y
# Then applies patches from tools/selectize-patches.

# This script can be sourced from RStudio, or run with Rscript.

# =============================================================================
# Download library
# =============================================================================

## First, download the main selectize.js and css
library(rprojroot)

version <- "0.12.4"
dest_dir <- rprojroot::find_package_root_file("inst/www/shared/selectize")
tag <- paste0("v", version)
dest_file <- file.path(tempdir(), paste0("selectize.js-", version, ".zip"))
url <- sprintf("https://github.com/selectize/selectize.js/archive/%s.zip", tag)

download.file(url, dest_file)
unzipped <- tempdir()
unzip(dest_file, exdir = unzipped)

unlink(dest_dir, recursive = TRUE)

dir.create(file.path(dest_dir, "js"), recursive = TRUE)
file.copy(
  file.path(unzipped, paste0("selectize.js-", version), "dist", "js", "standalone", "selectize.min.js"),
  file.path(dest_dir, "js"),
  overwrite = TRUE
)

dir.create(file.path(dest_dir, "css"), recursive = TRUE)
file.copy(
  file.path(unzipped, paste0("selectize.js-", version), "dist", "css", "selectize.bootstrap3.css"),
  file.path(dest_dir, "css"),
  overwrite = TRUE
)

## Second, download accessibility plugin
version <- "927d81e9ea86acac1724d57b2ce9f3c962fd34c4"
url <- sprintf("https://github.com/SLMNBJ/selectize-plugin-a11y/archive/%s.zip", version)
dest_file <- file.path(tempdir(), paste0("selectize-plugin-a11y-", version, ".zip"))
download.file(url, dest_file)

unzipped <- tempdir()
unzip(dest_file, exdir = unzipped)

dir.create(file.path(dest_dir, "accessibility", "js"), recursive = TRUE)
file.copy(
  file.path(unzipped, paste0("selectize-plugin-a11y-", version), "selectize-plugin-a11y.js"),
  file.path(dest_dir, "accessibility", "js"),
  overwrite = TRUE
)

# =============================================================================
# Apply patches
# =============================================================================
# The version of selectize-plugin-a11y that we use is modified from the base version
# in the following ways:
# * In our version, each option item has their own unique id to be announced to screen readers when selection changes.

patch_dir <- rprojroot::find_package_root_file("tools/selectize-patches")

for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch(
    {
      message(sprintf("Applying %s", basename(patch)))
      withr::with_dir(rprojroot::find_package_root_file(), system(sprintf("git apply %s", patch)))
    },
    error = function(e) {
      quit(save = "no", status = 1)
    }
  )
}

# =============================================================================
# Generate minified js
# =============================================================================
withr::with_dir(rprojroot::find_package_root_file("tools"), system("yarn build"))
