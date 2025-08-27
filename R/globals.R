# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # R's lazy-loading package scheme causes the private seed to be cached in the
  # package itself, making our PRNG completely deterministic. This line resets
  # the private seed during load.
  withPrivateSeed(set.seed(NULL))

  for (expr in on_load_exprs) {
    eval(expr, envir = environment(.onLoad))
  }

  # Make sure these methods are available to knitr if shiny is loaded but not
  # attached.
  s3_register("knitr::knit_print", "reactive")
  s3_register("knitr::knit_print", "shiny.appobj")
  s3_register("knitr::knit_print", "shiny.render.function")
}


on_load_exprs <- list()
# Register an expression to be evaluated when the package is loaded (in the
# .onLoad function).
on_load <- function(expr) {
  on_load_exprs[[length(on_load_exprs) + 1]] <<- substitute(expr)
}

on_load({
  IS_SHINY_LOCAL_PKG <- exists(".__DEVTOOLS__")
})
