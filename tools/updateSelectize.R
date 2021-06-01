#!/usr/bin/env Rscript
library(rprojroot)

## -----------------------------------------------------------------
## First, download the main selectize.js and css
## -----------------------------------------------------------------

version <- "0.12.4"
dest_dir <- find_package_root_file("inst/www/shared/selectize")
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

## -----------------------------------------------------------------
## Second, download accessibility plugin
## -----------------------------------------------------------------

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

tmpdir <- tempdir()

## -----------------------------------------------------------------
## Third, download Bootstrap 4 SASS port of selectize less
## This is using a specific sha because this hasn't been included in an official release
## https://github.com/papakay/selectize-bootstrap-4-style/pull/19
## -----------------------------------------------------------------

version <- "5013be4e97a14bef47bc8badcc78e6762815ef38"
zip_src <- sprintf("https://github.com/papakay/selectize-bootstrap-4-style/archive/%s.zip", version)
zip_target <- file.path(tmpdir, "select-bs4.zip")
download.file(zip_src, zip_target)
unzip(zip_target, exdir = dirname(zip_target))
target <- "inst/www/shared/selectize/scss"
unlink(target, recursive = TRUE)
dir.create(target)
file.rename(
  file.path(tmpdir, sprintf("selectize-bootstrap-4-style-%s/src/selectize", version)),
  target
)

# Remove the unnecessary imports of Bootstrap
scss_file <- "inst/www/shared/selectize/scss/selectize.bootstrap4.scss"
scss <- readLines(scss_file)
scss <- scss[!grepl('@import\\s+"\\.\\./bootstrap', scss)]
writeLines(scss, scss_file)

# Support Bootstrap 5 as well
# https://github.com/selectize/selectize.js/issues/1584
writeLines(
  c(
    "$input-line-height-sm: $form-select-line-height !default;",
    "@import 'selectize.bootstrap4';",
    ".selectize-control{padding:0;}"
  ),
  file.path(target, "selectize.bootstrap5.scss")
)

## -----------------------------------------------------------------
## Fourth, download Bootstrap 3 SASS port
## https://github.com/herschel666/selectize-scss
## Note that the base selectize.scss, as well as the plugins, are identical
## to the BS4 port, so we only need the selectize.bootstrap3.scss file
## -----------------------------------------------------------------

version <- "0.10.1"
zip_src <- sprintf("https://github.com/herschel666/selectize-scss/archive/v%s.zip", version)
zip_target <- file.path(tmpdir, "select-bs3.zip")
download.file(zip_src, zip_target)
unzip(zip_target, exdir = dirname(zip_target))
target <- "inst/www/shared/selectize/scss/selectize.bootstrap3.scss"
file.rename(
  file.path(tmpdir, sprintf("selectize-scss-%s/src/selectize.bootstrap3.scss", version)),
  target
)


# =============================================================================
# Apply patches
# =============================================================================
# The version of selectize-plugin-a11y that we use is modified from the base version
# in the following ways:
# * In our version, each option item has their own unique id to be announced to screen readers when selection changes.

patch_dir <- find_package_root_file("tools/selectize-patches")

for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch(
    {
      message(sprintf("Applying %s", basename(patch)))
      withr::with_dir(find_package_root_file(), system(sprintf("git apply %s", patch)))
    },
    error = function(e) {
      quit(save = "no", status = 1)
    }
  )
}

# =============================================================================
# Generate minified js
# =============================================================================
withr::with_dir(find_package_root_file("srcts"), system("yarn build"))
