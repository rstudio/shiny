

# Very similar to srcrefFromShinyCall(),
# however, this works when the function does not have a srcref attr set
otel_srcref_attributes <- function(x) {
  if (is.function(x)) {
    # Get the first srcref
    x <- getSrcRefs(x)[[1]][[1]]
    srcref <- attr(srcref, "srcref", exact = TRUE)
  }

  stopifnot(inherits(x, "srcref"))

  srcref <- x

  # Semantic conventions for code: https://opentelemetry.io/docs/specs/semconv/registry/attributes/code/
  #
  # Inspiration from https://github.com/r-lib/testthat/pull/2087/files#diff-92de3306849d93d6f7e76c5aaa1b0c037e2d716f72848f8a1c70536e0c8a1564R123-R124
  dropNulls(list(
    "code.filepath" = attr(srcref, "srcfile")$filename,
    "code.lineno" = srcref[1],
    "code.column" = srcref[2]
  ))
}
