version <- "3.4.1"

# Obtain Bootstrap source
tmpzip <- tempfile(fileext = ".zip")
download.file(
  sprintf("https://github.com/twbs/bootstrap/archive/v%s.zip", version),
  tmpzip
)
unzip(tmpzip, exdir = dirname(tmpzip))
bs_dir <- file.path(dirname(tmpzip), paste0("bootstrap-", version))

# Apply patch to Bootstrap source and re-generate distributed files
patch_dir <- rprojroot::find_package_root_file("tools/bootstrap-patches")
withr::with_dir(
  bs_dir,
  {
    for (patch in list.files(patch_dir, full.names = TRUE)) {
      tryCatch(
        {
          message(sprintf("Applying %s", basename(patch)))
          system(sprintf("git apply '%s'", patch))
        },
        error = function(e) {
          quit(save = "no", status = 1)
        }
      )
    }
    system("yarn install")
    system("grunt")
  }
)


# Copy over distributed files
target <- rprojroot::find_package_root_file("inst/www/shared/bootstrap")
file.copy(file.path(bs_dir, "dist", "js"), target, recursive = TRUE)
file.copy(file.path(bs_dir, "dist", "css"), target, recursive = TRUE)
file.copy(file.path(bs_dir, "dist", "fonts"), target, recursive = TRUE)
