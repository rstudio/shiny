# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # R's lazy-loading package scheme causes the private seed to be cached in the
  # package itself, making our PRNG completely deterministic. This line resets
  # the private seed during load.
  withPrivateSeed(set.seed(NULL))

  # Create this at the top level, but since the object is from a different
  # package, we don't want to bake it into the built binary package.
  restoreCtxStack <<- fastmap::faststack()

  # Make sure these methods are available to knitr if shiny is loaded but not
  # attached.
  register_s3_method("knitr", "knit_print", "reactive")
  register_s3_method("knitr", "knit_print", "shiny.appobj")
  register_s3_method("knitr", "knit_print", "shiny.render.function")

  # Shiny 1.4.0 bumps jQuery 1.x to 3.x, which caused a problem
  # with static-rendering of htmlwidgets, and htmlwidgets 1.5
  # includes a fix for this problem
  # https://github.com/rstudio/shiny/issues/2630
  register_upgrade_message("htmlwidgets", 1.5)
}
