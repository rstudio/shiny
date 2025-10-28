

# Very similar to srcrefFromShinyCall(),
# however, this works when the function does not have a srcref attr set
otel_srcref_attributes <- function(srcref) {
  if (is.function(srcref)) {
    srcref <- getSrcRefs(srcref)[[1]][[1]]
  }

  if (is.null(srcref)) {
    return(NULL)
  }

  stopifnot(inherits(srcref, "srcref"))

  # Semantic conventions for code: https://opentelemetry.io/docs/specs/semconv/registry/attributes/code/
  #
  # Inspiration from https://github.com/r-lib/testthat/pull/2087/files#diff-92de3306849d93d6f7e76c5aaa1b0c037e2d716f72848f8a1c70536e0c8a1564R123-R124
  dropNulls(list(
    "code.filepath" = attr(srcref, "srcfile")$filename,
    "code.lineno" = srcref[1],
    "code.column" = srcref[2]
  ))
}

#' Get the srcref for the call at the specified stack level
#'
#' If you need to go farther back in the `sys.call()` stack, supply a larger
#' negative number to `which_offset`. The default of 0 gets the immediate
#' caller. `-1` would get the caller's caller, and so on.
#' @param which_offset The stack level to get the call from. Defaults to -1 (the
#'   immediate caller).
#' @return An srcref object, or NULL if none is found.
#' @noRd
get_call_srcref <- function(which_offset = 0) {
  # Go back one call to account for this function itself
  call <- sys.call(which_offset - 1)

  srcref <- attr(call, "srcref", exact = TRUE)
  srcref
}


append_otel_attrs <- function(attrs, new_attrs) {
  if (is.null(new_attrs)) {
    return(attrs)
  }

  attrs[names(new_attrs)] <- new_attrs

  attrs
}

append_otel_srcref_attrs <- function(attrs, call_srcref) {
  if (is.null(call_srcref)) {
    return(attrs)
  }

  srcref_attrs <- otel_srcref_attributes(call_srcref)
  attrs <- append_otel_attrs(attrs, srcref_attrs)

  attrs
}
