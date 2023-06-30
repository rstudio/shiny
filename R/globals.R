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
  s3_register("knitr::knit_print", "reactive")
  s3_register("knitr::knit_print", "shiny.appobj")
  s3_register("knitr::knit_print", "shiny.render.function")
}
