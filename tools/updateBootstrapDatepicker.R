#!/usr/bin/env Rscript
# Retrieves a particular version of bootstrap-datepicker:
#  https://github.com/uxsolutions/bootstrap-datepicker
# After retrieving, you can apply patches stored in
# tools/datepicker-patches with applyDatepickerPatches.R

library(rprojroot)

version   <- "1.9.0"
dest_dir  <- rprojroot::find_package_root_file("inst/www/shared/datepicker")
tag       <- paste0("v", version)
dest_file <- file.path(tempdir(), paste0("bootstrap-datepicker-", version, ".zip"))
url       <- sprintf("https://github.com/uxsolutions/bootstrap-datepicker/archive/%s.zip", tag)

download.file(url, dest_file)
unzipped <- tempdir()
unzip(dest_file, exdir = unzipped)
src_dir <- file.path(unzipped, paste0("bootstrap-datepicker-", version))

unlink(dest_dir, recursive = TRUE)

dir.create(file.path(dest_dir, "js"), recursive = TRUE)
file.copy(
  dir(file.path(src_dir, "dist", "js"), full.names = TRUE),
  file.path(dest_dir, "js"),
  overwrite = TRUE
)

dir.create(file.path(dest_dir, "js", "locales"), recursive = TRUE)
file.copy(
  dir(file.path(src_dir, "dist", "locales"), "\\.js$", full.names = TRUE),
  file.path(dest_dir, "js", "locales"),
  overwrite = TRUE
)

# Create a target directory for Sass source
scss_dir <- file.path(dest_dir, "scss")
dir.create(scss_dir, recursive = TRUE)

# Grab less source files & convert to sass
# npm install -g less2sass
src <- file.path(unzipped, paste0("bootstrap-datepicker-", version))
less_files <- dir(src, pattern = "\\.less$", full.names = TRUE, recursive = TRUE)
invisible(lapply(less_files, function(file) {
  system(paste("less2sass", file))
}))

# Copy over just the bootstrap
sass_files <- file.path(src, c("less/datepicker3.scss", "build/build3.scss"))
file.copy(sass_files, scss_dir)

# Fix relative imports
scss <- dir(file.path(dest_dir, "scss"), full.names = TRUE)
invisible(lapply(
  scss, function(x) {
    txt <- readLines(x)
    txt <- sub('^@import "../less/', '@import "', txt)
    txt <- sub('^@import "../build/', '@import "', txt)
    writeLines(txt, x)
  }
))


# Apply patches to source
patch_dir <- rprojroot::find_package_root_file("tools/datepicker-patches")
for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch(
    {
      message(sprintf("Applying %s", basename(patch)))
      system(sprintf("git apply %s", patch))
    },
    error = function(e) quit(save = "no", status = 1)
  )
}

# Compile to CSS
library(sass)
library(bslib)
css_dir <- file.path(dest_dir, "css")
dir.create(css_dir, recursive = TRUE)
sass_partial(
  sass_file(file.path(dest_dir, "scss", "build3.scss")),
  bundle = bs_theme(),
  output = file.path(css_dir, "bootstrap-datepicker3.css"),
  write_attachments = FALSE
)
sass_partial(
  sass_file(file.path(dest_dir, "scss", "build3.scss")),
  bundle = bs_theme(),
  output = file.path(css_dir, "bootstrap-datepicker3.min.css"),
  options = sass_options(output_style = "compressed"),
  write_attachments = FALSE
)
