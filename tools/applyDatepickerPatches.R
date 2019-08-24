#!/usr/bin/env Rscript
# Applies patches stored in tools/datepicker-patches
# Should be run after running tools/updateBootstrapDatepicker.R

library(rprojroot)

patch_dir <- rprojroot::find_package_root_file("tools/datepicker-patches")

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
