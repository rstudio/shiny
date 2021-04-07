#!/usr/bin/env Rscript
library(rprojroot)
library(withr)
library(brio)

## -----------------------------------------------------------------
## First, download the main selectize.js and css
## -----------------------------------------------------------------

version <- "0.13.3"
tag <- paste0("v", version)
url <- sprintf("https://github.com/selectize/selectize.js/archive/%s.zip", tag)
zip <- file.path(tempdir(), paste0("selectize.js-", version, ".zip"))

download.file(url, zip)
unzip(zip, exdir = tempdir())

target <- find_package_root_file("inst/www/shared/selectize")
unlink(target, recursive = TRUE)
dir.create(target, recursive = TRUE)

with_dir(target, {
  lapply(c("js", "css", "scss", "plugins"), dir.create)
})

with_dir(tempdir(), {
  root <- paste0("selectize.js-", version)
  file.copy(
    file.path(root, "dist", "js", "standalone", "selectize.min.js"),
    file.path(target, "js")
  )
  file.copy(
    file.path(root, "dist", "css", "selectize.bootstrap3.css"),
    file.path(target, "css")
  )
  file.copy(
    file.path(root, "src", "scss"),
    target, recursive = TRUE
  )
  file.copy(
    file.path(root, "src", "plugins"),
    target, recursive = TRUE
  )
  plugin_files <- dir(file.path(target, "plugins"), recursive = TRUE, full.names = TRUE)
  file.remove(plugin_files[!grepl("\\.scss$", plugin_files)])
})

# Remove all Sass imports of Bootstrap since we handle that via bslib
sass_files <- dir(target, pattern = "\\.scss$", recursive = TRUE, full.names = TRUE)
lapply(sass_files, function(f) {
  src <- readLines(f)
  src <- src[!grepl("@import .*/node_modules/bootstrap", src)]
  writeLines(src, f)
})


## -----------------------------------------------------------------
## Second, download accessibility plugin
## -----------------------------------------------------------------

version <- "927d81e9ea86acac1724d57b2ce9f3c962fd34c4"
url <- sprintf("https://github.com/SLMNBJ/selectize-plugin-a11y/archive/%s.zip", version)
zip <- file.path(tempdir(), paste0("selectize-plugin-a11y-", version, ".zip"))

download.file(url, zip)
unzip(zip, exdir = tempdir())

with_dir(target, {
  dir.create("accessibility/js", recursive = TRUE)
})

with_dir(tempdir(), {
  file.copy(
    file.path(paste0("selectize-plugin-a11y-", version), "selectize-plugin-a11y.js"),
    file.path(target, "accessibility/js")
  )
})

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
withr::with_dir(find_package_root_file("tools"), system("yarn grunt"))
