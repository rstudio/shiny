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

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

register_upgrade_message <- function(pkg, version) {

  msg <- sprintf(
    "This version of Shiny is designed to work with '%s' >= %s.
    Please upgrade via install.packages('%s').",
    pkg, version, pkg
  )

  if (pkg %in% loadedNamespaces() && !is_available(pkg, version)) {
    packageStartupMessage(msg)
  }

  # Always register hook in case pkg is loaded at some
  # point the future (or, potentially, but less commonly,
  # unloaded & reloaded)
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      if (!is_available(pkg, version)) packageStartupMessage(msg)
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

  # Shiny 1.4.0 bumps jQuery 1.x to 3.x, which caused a problem
  # with static-rendering of htmlwidgets, and htmlwidgets 1.5
  # includes a fix for this problem
  # https://github.com/rstudio/shiny/issues/2630
  register_upgrade_message("htmlwidgets", 1.5)
}
