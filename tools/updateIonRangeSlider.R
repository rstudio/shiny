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
sass_files <- dir(file.path(src, "less"), pattern = "scss$", full.names = TRUE, recursive = TRUE)

invisible(lapply(sass_files, function(file) {
  txt <- readLines(file)
  # Add !default flags to variable definitions
  txt <- sub("(\\$.*:.*);", "\\1 !default;", txt)
  # less2sass misses this bit
  txt <- sub("@import (reference)", "@import", txt, fixed = TRUE)
  writeLines(txt, file)
}))

# Move over less/sass
file.rename(
  file.path(src, "less"),
  file.path(target, "scss")
)
# Cleanup
file.remove(
  dir(target, pattern = "less$", full.names = TRUE, recursive = TRUE)
)


# Apply git patches *before* compiling skin Sass -> CSS (so shiny skin is included)
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
css_home <- file.path(target, "css")
dir.create(css_home)

# Compile the 'base' Sass to CSS
scss_home <- file.path(target, "scss")
sass(
  sass_file(file.path(scss_home, "_base.scss")),
  output = file.path(css_home, "ion.rangeSlider.css")
)

# Compile each skin
skin_home <- file.path(scss_home, "skins")
for (skin_scss in dir(skin_home, full.names = TRUE)) {
  skin_name <- tools::file_path_sans_ext(basename(skin_scss))
  sass(
    list(
      sass_file(file.path(scss_home, "_variables.scss")),
      sass_file(skin_scss)
    ),
    output = file.path(
      css_home, sprintf("ion.rangeSlider.skin%s.css", tools::toTitleCase(skin_name))
    )
  )
}


# Finally, run yarn build so the JS patches propogate to the minified files
withr::with_dir(rprojroot::find_package_root_file("tools"), system("yarn build"))
