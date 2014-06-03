# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # R's lazy-loading package scheme causes the private seed to be cached in the
  # package itself, making our PRNG completely deterministic. This line resets
  # the private seed during load.
  withPrivateSeed(reinitializeSeed())
}
