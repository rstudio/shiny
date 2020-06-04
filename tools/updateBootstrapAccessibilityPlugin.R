#!/usr/bin/env Rscript
# Retrieves a particular version of bootstrap-accessibility-plugin:
#  https://github.com/paypal/bootstrap-accessibility-plugin
# Since the last version release has been too behind the latest commit, we will use a specific sha instead.


library(rprojroot)

sha   <- "fbbf870eafc1ee5d4547fabbeea71778ddbe2166"
dest_dir  <- rprojroot::find_package_root_file("inst/www/shared/bootstrap")

js_url       <- sprintf("https://raw.githubusercontent.com/paypal/bootstrap-accessibility-plugin/%s/plugins/js/bootstrap-accessibility.js", sha)
min_js_url       <- sprintf("https://raw.githubusercontent.com/paypal/bootstrap-accessibility-plugin/%s/plugins/js/bootstrap-accessibility.min.js", sha)
css_url       <- sprintf("https://raw.githubusercontent.com/paypal/bootstrap-accessibility-plugin/%s/plugins/css/bootstrap-accessibility.css", sha)
url <- c(js_url, min_js_url, css_url)

# Downloading each required file:
lapply(url, function(x) {download.file(x, file.path(tempdir(), basename(x)))})

# Copying js files:
dir.create(file.path(dest_dir, "js"), recursive = TRUE)
file.copy(
  file.path(tempdir(), c("bootstrap-accessibility.js", "bootstrap-accessibility.min.js")),
  file.path(dest_dir, "js"),
  overwrite = TRUE
)

# Copying css file:
dir.create(file.path(dest_dir, "css"), recursive = TRUE)
file.copy(
  file.path(tempdir(), "bootstrap-accessibility.css"),
  file.path(dest_dir, "css"),
  overwrite = TRUE
)
