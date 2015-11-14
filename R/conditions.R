# These two functions only exist to create artifacts on the stack trace
# (sys.call()) that instruct the stack trace pretty printer what parts of
# the stack trace are interesting or not. The initial state is 1 and we
# walk from the outermost call inwards. Each ..stacktraceoff.. decrements
# the state by one, and each ..stacktraceon.. increments the state by one.
# Any stack trace frame whose value is less than 1 is hidden, and finally,
# the ..stacktraceon.. and ..stacktraceoff.. calls themselves are hidden
# too.
#
# Example:
# ..stacktraceoff..({
#   tryCatch(
#     withVisible(..stacktraceon..(userFunc()))
#   )
# })
..stacktraceon.. <- function(x) x
..stacktraceoff.. <- function(x) x

getCallNames <- function(calls) {
  sapply(calls, function(call) {
    if (is.function(call[[1]])) {
      "<Anonymous>"
    } else if (inherits(call[[1]], "call")) {
      format(call[[1]])
    } else {
      paste0(as.character(call[[1]]), collapse = " ")
    }
  })
}

getLocs <- function(calls) {
  sapply(calls, function(call) {
    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      if (!is.null(srcfile) && !is.null(srcfile$filename)) {
        loc <- paste0(srcfile$filename, "#", srcref[[1]])
        return(paste0(" [", loc, "]"))
      }
    }
    return("")
  })
}

captureStackTraces <- function(expr,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  withCallingHandlers(expr,
    error = function(e) {
      if (is.null(attr(e, "stack.trace", exact = TRUE))) {
        calls <- sys.calls()

        srcrefs <- getSrcRefs(calls)
        if (offset) {
          # Offset calls vs. srcrefs by 1 to make them more intuitive.
          # E.g. for "foo [bar.R:10]", line 10 of bar.R will be part of
          # the definition of foo().
          srcrefs <- c(tail(srcrefs, -1), list(NULL))
        }
        calls <- setSrcRefs(calls, srcrefs)

        callnames <- getCallNames(calls)

        # Hide and show parts of the callstack based on ..stacktrace(on|off)..
        if (full) {
          toShow <- rep.int(TRUE, length(calls))
        } else {
          # Remove stop(), .handleSimpleError(), and h() calls from the end of
          # the calls--they don't add any helpful information. But only remove
          # the last *contiguous* block of them, and then, only if they are the
          # last thing in the calls list.
          hideable <- callnames %in% c("stop", ".handleSimpleError", "h")
          # What's the last that *didn't* match stop/.handleSimpleError/h?
          lastGoodCall <- max(which(!hideable))
          toRemove <- length(calls) - lastGoodCall
          # But don't remove more than 5 levels--that's an indication we might
          # have gotten it wrong, I guess
          if (toRemove > 0 && toRemove < 5) {
            calls <- head(calls, -toRemove)
            callnames <- head(callnames, -toRemove)
          }

          # This uses a ref-counting scheme. It might make sense to switch this
          # to a toggling scheme, so the most recent ..stacktrace(on|off)..
          # directive wins, regardless of what came before it.
          # Also explicitly remove ..stacktraceon.. because it can appear with
          # score > 0 but still should never be shown.
          score <- rep.int(0, length(callnames))
          score[callnames == "..stacktraceoff.."] <- -1
          score[callnames == "..stacktraceon.."] <- 1
          toShow <- (1 + cumsum(score)) > 0 & !(callnames %in% c("..stacktraceon..", "..stacktraceoff.."))
        }
        calls <- calls[toShow]

        calls <- rev(calls) # Show in traceback() order
        index <- rev(which(toShow))
        width <- floor(log10(max(index))) + 1
        attr(e, "stack.trace") <- paste0(collapse = "\n",
          "    ",
          formatC(index, width = width),
          ": ",
          getCallNames(calls),
          getLocs(calls)
        )
        stop(e)
      }
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

# Capture stack traces and log errors, but allow errors
# to propagate beyond this point (i.e. doesn't catch).
withLogErrors <- function(expr) {
  withCallingHandlers(
    captureStackTraces(expr),
    error = printError
  )
}

printError <- function(cond) {
  cat(file = stderr(), sprintf("Error in %s: %s\n",
    getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
  printStackTrace(cond)
}

printStackTrace <- function(cond) {
  stackTrace <- attr(cond, "stack.trace", exact = TRUE)
  if (!is.null(stackTrace)) {
    cat(file = stderr(), "Stack trace (innermost first):\n")
    cat(file = stderr(), stackTrace, "\n")
  } else {
    cat(file = stderr(), "No stack trace available")
  }
}

stripStackTrace <- function(cond) {
  attr(cond, "stack.trace") <- NULL
  cond
}
