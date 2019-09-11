# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

.onLoad <- function(libname, pkgname) {
  # R's lazy-loading package scheme causes the private seed to be cached in the
  # package itself, making our PRNG completely deterministic. This line resets
  # the private seed during load.
  withPrivateSeed(set.seed(NULL))

  # Make sure these methods are available to knitr if shiny is loaded but not
  # attached.
  register_s3_method("knitr", "knit_print", "reactive")
  register_s3_method("knitr", "knit_print", "shiny.appobj")
  register_s3_method("knitr", "knit_print", "shiny.render.function")
}

.onAttach <- function(libname, pkgname) {
  # Check for htmlwidgets version, if installed. As of Shiny 0.12.0 and
  # htmlwidgets 0.4, both packages switched from RJSONIO to jsonlite. Because of
  # this change, Shiny 0.12.0 will work only with htmlwidgets >= 0.4, and vice
  # versa.
  if (system.file(package = "htmlwidgets") != "" &&
      utils::packageVersion("htmlwidgets") < "0.4") {
    packageStartupMessage(
      "This version of Shiny is designed to work with htmlwidgets >= 0.4. ",
      "Please upgrade your version of htmlwidgets."
    )
  }
}
