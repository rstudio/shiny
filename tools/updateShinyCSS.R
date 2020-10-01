#!/usr/bin/env Rscript
library(rprojroot)
library(sass)

scss <- find_package_root_file("inst/www/shared/shiny_scss/shiny.scss")
shiny_css <- sass(
  sass_file(scss),
  options = sass_options(output_style = "compressed"),
  output = find_package_root_file("inst/www/shared/shiny.min.css")
)
