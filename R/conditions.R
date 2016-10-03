#' Stack trace manipulation functions
#'
#' Advanced (borderline internal) functions for capturing, printing, and
#' manipulating stack traces.
#'
#' @return \code{printError} and \code{printStackTrace} return
#'   \code{invisible()}. The other functions pass through the results of
#'   \code{expr}.
#'
#' @examples
#' # Keeps tryCatch and withVisible related calls off the
#' # pretty-printed stack trace
#'
#' visibleFunction1 <- function() {
#'   stop("Kaboom!")
#' }
#'
#' visibleFunction2 <- function() {
#'   visibleFunction1()
#' }
#'
#' hiddenFunction <- function(expr) {
#'   expr
#' }
#'
#' # An example without ..stacktraceon/off.. manipulation.
#' # The outer "try" is just to prevent example() from stopping.
#' try({
#'   # The withLogErrors call ensures that stack traces are captured
#'   # and that errors that bubble up are logged using warning().
#'   withLogErrors({
#'     # tryCatch and withVisible are just here to add some noise to
#'     # the stack trace.
#'     tryCatch(
#'       withVisible({
#'         hiddenFunction(visibleFunction2())
#'       })
#'     )
#'   })
#' })
#'
#' # Now the same example, but with ..stacktraceon/off.. to hide some
#' # of the less-interesting bits (tryCatch and withVisible).
#' ..stacktraceoff..({
#'   try({
#'     withLogErrors({
#'       tryCatch(
#'         withVisible(
#'           hiddenFunction(
#'             ..stacktraceon..(visibleFunction2())
#'           )
#'         )
#'       )
#'     })
#'   })
#' })
#'
#'
#' @name stacktrace
#' @rdname stacktrace
#' @keywords internal
NULL

getCallNames <- function(calls) {
  sapply(calls, function(call) {
    if (is.function(call[[1]])) {
      "<Anonymous>"
    } else if (inherits(call[[1]], "call")) {
      paste0(format(call[[1]]), collapse = " ")
    } else if (typeof(call[[1]]) == "promise") {
      "<Promise>"
    } else {
      paste0(as.character(call[[1]]), collapse = " ")
    }
  })
}

getLocs <- function(calls) {
  vapply(calls, function(call) {
    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      if (!is.null(srcfile) && !is.null(srcfile$filename)) {
        loc <- paste0(srcfile$filename, "#", srcref[[1]])
        return(paste0(" [", loc, "]"))
      }
    }
    return("")
  }, character(1))
}

#' @details \code{captureStackTraces} runs the given \code{expr} and if any
#'   \emph{uncaught} errors occur, annotates them with stack trace info for use
#'   by \code{printError} and \code{printStackTrace}. It is not necessary to use
#'   \code{captureStackTraces} around the same expression as
#'   \code{withLogErrors}, as the latter includes a call to the former. Note
#'   that if \code{expr} contains calls (either directly or indirectly) to
#'   \code{try}, or \code{tryCatch} with an error handler, stack traces therein
#'   cannot be captured unless another \code{captureStackTraces} call is
#'   inserted in the interior of the \code{try} or \code{tryCatch}. This is
#'   because these calls catch the error and prevent it from traveling up to the
#'   condition handler installed by \code{captureStackTraces}.
#'
#' @param expr The expression to wrap.
#' @rdname stacktrace
#' @export
captureStackTraces <- function(expr) {
  withCallingHandlers(expr,
    error = function(e) {
      if (is.null(attr(e, "stack.trace", exact = TRUE))) {
        calls <- sys.calls()
        attr(e, "stack.trace") <- calls
        stop(e)
      }
    }
  )
}

#' @details \code{withLogErrors} captures stack traces and logs errors that
#'   occur in \code{expr}, but does allow errors to propagate beyond this point
#'   (i.e. it doesn't catch the error). The same caveats that apply to
#'   \code{captureStackTraces} with regard to \code{try}/\code{tryCatch} apply
#'   to \code{withLogErrors}.
#' @rdname stacktrace
#' @export
withLogErrors <- function(expr,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  withCallingHandlers(
    captureStackTraces(expr),
    error = function(cond) {
      # Don't print shiny.silent.error (i.e. validation errors)
      if (inherits(cond, "shiny.silent.error")) return()
      if (isTRUE(getOption("show.error.messages"))) {
        printError(cond, full = full, offset = offset)
      }
    }
  )
}

#' @details \code{printError} prints the error and stack trace (if any) using
#'   \code{warning(immediate.=TRUE)}. \code{printStackTrace} prints the stack
#'   trace only.
#'
#' @param cond An condition object (generally, an error).
#' @param full If \code{TRUE}, then every element of \code{sys.calls()} will be
#'   included in the stack trace. By default (\code{FALSE}), calls that Shiny
#'   deems uninteresting will be hidden.
#' @param offset If \code{TRUE} (the default), srcrefs will be reassigned from
#'   the calls they originated from, to the destinations of those calls. If
#'   you're used to stack traces from other languages, this feels more
#'   intuitive, as the definition of the function indicated in the call and the
#'   location specified by the srcref match up. If \code{FALSE}, srcrefs will be
#'   left alone (traditional R treatment where the srcref is of the callsite).
#' @rdname stacktrace
#' @export
printError <- function(cond,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s",
    getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
  printStackTrace(cond, full = full, offset = offset)
  invisible()
}

#' @rdname stacktrace
#' @export
printStackTrace <- function(cond,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  stackTrace <- attr(cond, "stack.trace", exact = TRUE)
  tryCatch(
    if (!is.null(stackTrace)) {
      message(paste0(
        "Stack trace (innermost first):\n",
        paste0(collapse = "\n",
          formatStackTrace(stackTrace, full = full, offset = offset,
            indent = "    ")
        )
      ))
    } else {
      message("No stack trace available")
    },

    error = function(cond) {
      warning("Failed to write stack trace: ", cond)
    }
  )
  invisible()
}

#' @details \code{extractStackTrace} takes a list of calls (e.g. as returned
#'   from \code{conditionStackTrace(cond)}) and returns a data frame with one
#'   row for each stack frame and the columns \code{num} (stack frame number),
#'   \code{call} (a function name or similar), and \code{loc} (source file path
#'   and line number, if available).
#' @rdname stacktrace
#' @export
extractStackTrace <- function(calls,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  srcrefs <- getSrcRefs(calls)
  if (offset) {
    # Offset calls vs. srcrefs by 1 to make them more intuitive.
    # E.g. for "foo [bar.R:10]", line 10 of bar.R will be part of
    # the definition of foo().
    srcrefs <- c(utils::tail(srcrefs, -1), list(NULL))
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
      calls <- utils::head(calls, -toRemove)
      callnames <- utils::head(callnames, -toRemove)
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

  data.frame(
    num = index,
    call = getCallNames(calls),
    loc = getLocs(calls),
    stringsAsFactors = FALSE
  )
}

#' @details \code{formatStackTrace} is similar to \code{extractStackTrace}, but
#'   it returns a preformatted character vector instead of a data frame.
#' @param indent A string to prefix every line of the stack trace.
#' @rdname stacktrace
#' @export
formatStackTrace <- function(calls, indent = "    ",
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  st <- extractStackTrace(calls, full = full, offset = offset)
  if (nrow(st) == 0) {
    return(character(0))
  }

  width <- floor(log10(max(st$num))) + 1
  paste0(
    indent,
    formatC(st$num, width = width),
    ": ",
    st$call,
    st$loc
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

stripStackTrace <- function(cond) {
  conditionStackTrace(cond) <- NULL
}

#' @details \code{conditionStackTrace} and \code{conditionStackTrace<-} are
#'   accessor functions for getting/setting stack traces on conditions.
#'
#' @param cond A condition that may have previously been annotated by
#'   \code{captureStackTraces} (or \code{withLogErrors}).
#' @rdname stacktrace
#' @export
conditionStackTrace <- function(cond) {
  attr(cond, "stack.trace", exact = TRUE)
}

#' @param value The stack trace value to assign to the condition.
#' @rdname stacktrace
#' @export
`conditionStackTrace<-` <- function(cond, value) {
  attr(cond, "stack.trace") <- value
  invisible(cond)
}

#' @details The two functions \code{..stacktraceon..} and
#'   \code{..stacktraceoff..} have no runtime behavior during normal execution;
#'   they exist only to create artifacts on the stack trace (sys.call()) that
#'   instruct the stack trace pretty printer what parts of the stack trace are
#'   interesting or not. The initial state is 1 and we walk from the outermost
#'   call inwards. Each ..stacktraceoff.. decrements the state by one, and each
#'   ..stacktraceon.. increments the state by one. Any stack trace frame whose
#'   value is less than 1 is hidden, and finally, the ..stacktraceon.. and
#'   ..stacktraceoff.. calls themselves are hidden too.
#'
#' @rdname stacktrace
#' @export
..stacktraceon.. <- function(expr) expr
#' @rdname stacktrace
#' @export
..stacktraceoff.. <- function(expr) expr
