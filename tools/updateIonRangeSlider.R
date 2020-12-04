tmpdir <- tempdir()

# https://github.com/IonDen/ion.rangeSlider
version <- "2.3.1"
zip_src <- sprintf("https://github.com/IonDen/ion.rangeSlider/archive/%s.zip", version)
zip_target <- file.path(tmpdir, "ion.zip")
download.file(zip_src, zip_target)
unzip(zip_target, exdir = dirname(zip_target))
src <- file.path(dirname(zip_target), paste0("ion.rangeSlider-", version))
target <- "inst/www/shared/ionrangeslider"
unlink(target, recursive = TRUE)
dir.create(target)
# Move over JS files
file.rename(
  file.path(src, "js"),
  file.path(target, "js")
)

# Grab less src files and convert to sass
# npm install -g less2sass
less_files <- dir(file.path(src, "less"), full.names = TRUE, recursive = TRUE)
invisible(lapply(less_files, function(file) {
  system(paste("less2sass", file))
}))

# Copy over only the base (i.e., core) scss that we need for the shiny skin
dir.create(file.path(target, "scss"))
file.copy(
  file.path(src, "less", c("_base.scss", "_mixins.scss")),
  file.path(target, "scss", c("_base.scss", "_mixins.scss"))
)

# less2sass conversion doesn't convert this import correctly
base_css <- file.path(target, "scss", "_base.scss")
writeLines(
  sub("@import (reference)", "@import", readLines(base_css), fixed = TRUE),
  base_css
)

# Apply git patches *before* compiling skin Sass -> CSS (this should add the shiny skin)
patch_dir <- rprojroot::find_package_root_file("tools/ion.rangeSlider-patches")
for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch({
    message(sprintf("Applying %s", basename(patch)))
    withr::with_dir(rprojroot::find_package_root_file(), system(sprintf("git apply %s", patch)))
  },
    error = function(e) {
      quit(save = "no", status = 1)
    }
  )
}


# Now compile Sass -> CSS so that if the default styles are requested, we
# can serve them up without compilation (The distributed CSS includes all
# the skins in the same CSS file, but we want them split up)
library(sass)
dir.create(file.path(target, "css"))
sass(
  list(
    sass::sass_file(system.file("sass-utils/color-contrast.scss", package = "bslib")),
    sass_file(file.path(target, "scss", "shiny.scss"))
  ),
  output = file.path(target, "css", "ion.rangeSlider.css")
)


# Finally, run yarn build so the JS patches propogate to the minified files
withr::with_dir(rprojroot::find_package_root_file("tools"), system("yarn build"))
