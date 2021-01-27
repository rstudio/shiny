#!/usr/bin/env Rscript
# Copies over source from https://github.com/paypal/bootstrap-accessibility-plugin
#  that obtain and patch in https://github.com/rstudio/bslib
library(rprojroot)
library(sass)

if (packageVersion("bslib") < "0.2.4") stop ("`bslib >= 0.2.4` is required to update a11y plugin")

src <- system.file("lib/bs-a11y-p/", package = "bslib")
target <- find_package_root_file("inst/www/shared/bootstrap/accessibility/")

# bslib makes hover/focus outlines transparent by default, which is what we want to do
# to avoid such a jarring difference in visual appearance for things like tabsetPanel()
sass(
  sass_file(file.path(src, "src", "sass", "bootstrap-accessibility.scss")),
  output = file.path(target, "css", "bootstrap-accessibility.min.css"),
  options = sass_options(output_style = "compressed")
)

# bslib also patches the JS source to make sure it runs when the document is ready
# instead of executing immediately
file.copy(
  file.path(src, "plugins/js/bootstrap-accessibility.min.js"),
  file.path(target, "js/bootstrap-accessibility.min.js"),
  overwrite = TRUE
)
