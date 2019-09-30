#!/usr/bin/env Rscript
# Retrieves a particular version of ion.rangeSlider:
#  https://github.com/IonDen/ion.rangeSlider
# Then applies patches from tools/ion.rangeSlider-patches.

# This script can be sourced from RStudio, or run with Rscript.

# =============================================================================
# Download library
# =============================================================================
# TODO: In the future, use a tag instead of a bare commit hash. See
# updateBootstrapDatepicker.R for an example. This commit happened to be the
# tip of the development tree when we used it.
version   <- "f0e1116b065c3d091a9c5f04477aaf8894974e2a"
dest_dir  <- rprojroot::find_package_root_file("inst/www/shared/ionrangeslider")
dest_file <- file.path(tempdir(), paste0("ionrangeslider-", version, ".zip"))
url       <- sprintf("https://github.com/IonDen/ion.rangeSlider/archive/%s.zip", version)

download.file(url, dest_file)
unzip(dest_file, exdir = tempdir())
unzip_dir <- file.path(tempdir(), paste0("ion.rangeSlider-", version))

unlink(dest_dir, recursive = TRUE)

dir.create(file.path(dest_dir, "js"), recursive = TRUE)
dir.create(file.path(dest_dir, "css"), recursive = TRUE)
dir.create(file.path(dest_dir, "img"), recursive = TRUE)

file.copy(file.path(unzip_dir, "js/ion.rangeSlider.js"), file.path(dest_dir, "js"),
  overwrite = TRUE)
file.copy(file.path(unzip_dir, "css"), dest_dir, recursive = TRUE)
file.copy(file.path(unzip_dir, "img"), dest_dir, recursive = TRUE)


# =============================================================================
# Apply patches
# =============================================================================
# The version of ion.rangeSlider that we use is modified from the base version
# in the following ways:
# * In our version, mouse events on the slider are not propagated to lower
#   layers (#711, #1630).
# * We include a custom skin for Shiny.
# * When .update() is called, the `.removeProp("tabindex")` is wrapped in
#   try-catch. This is because this line results in an error with jQuery 3 and
#   PhantomJS. (#2587)

patch_dir <- rprojroot::find_package_root_file("tools/ion.rangeSlider-patches")

for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch({
    message(sprintf("Applying %s", basename(patch)))
    system(sprintf("git apply '%s'", patch))
  },
    error = function(e) {
      quit(save = "no", status = 1)
    }
  )
}
