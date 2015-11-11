# Formats sys.calls() into a nicer looking stack trace
prettyStackTrace <- function(calls) {
  paste0(collapse = "\n",
    sapply(rev(calls), function(call) {
      srcref <- attr(call, "srcref", exact = TRUE)
      pretty <- if (is.function(call[[1]])) {
        paste0("    <Anonymous>")
      } else if (inherits(call[[1]], "call")) {
        paste0("    ", format(call[[1]]))
      } else {
        paste0("    ", as.character(call[[1]]))
      }
      if (!is.null(srcref)) {
        srcfile <- attr(srcref, "srcfile", exact = TRUE)
        if (!is.null(srcfile) && !is.null(srcfile$filename)) {
          loc <- paste0(srcfile$filename, ":", srcref[[1]])
          pretty <- paste0(pretty, " [", loc, "]")
        }
      }
      pretty
    })
  )
}

captureStackTraces <- function(expr, truncate = 0) {
  withCallingHandlers(expr,
    error = function(e) {
      if (is.null(attr(e, "stack.trace", exact = TRUE))) {
        calls <- sys.calls()

        # Offset calls vs. srcrefs by 1 to make them more intuitive.
        # E.g. for "foo [bar.R:10]", line 10 of bar.R will be part of
        # the definition of foo().
        srcrefs <- c(tail(getSrcRefs(calls), -1), list(NULL))
        calls <- setSrcRefs(calls, srcrefs)

        calls <- head(calls, -3)
        if (!is.null(truncate) && !identical(truncate, FALSE)) {
          # truncate means we'll drop parts of the stack trace that
          # originate from withCallingHandlers and anything outside
          # of that
          cst <- as.symbol("captureStackTraces")
          pos <- Position(function(x) {
            identical(x[[1]], cst)
          }, calls, right = FALSE, nomatch = NA)
          if (!is.na(pos)) {
            # The "+2" here is for captureStackTraces and withCallingHandlers
            calls <- calls[(pos + 2 + truncate):length(calls)]
          }
        }
        attr(e, "stack.trace") <- prettyStackTrace(calls)
      }
      stop(e)
    }
  )
}

getSrcRefs <- function(calls) {
  lapply(calls, function(call) {
    attr(call, "srcref", exact = TRUE)
  })
}

setSrcRefs <- function(calls, srcrefs) {
  mapply(function(call, srcref) {
    structure(call, srcref = srcref)
  }, calls, srcrefs)
}
