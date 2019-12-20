#' Stack trace manipulation functions
#'
#' Advanced (borderline internal) functions for capturing, printing, and
#' manipulating stack traces.
#'
#' @return `printError` and `printStackTrace` return
#'   `invisible()`. The other functions pass through the results of
#'   `expr`.
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

getCallCategories <- function(calls) {
  vapply(calls, function(call) {
    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      if (!is.null(srcfile)) {
        if (!is.null(srcfile$original)) {
          return("pkg")
        } else {
          return("user")
        }
      }
    }
    return("")
  }, character(1))
}

#' @details `captureStackTraces` runs the given `expr` and if any
#'   *uncaught* errors occur, annotates them with stack trace info for use
#'   by `printError` and `printStackTrace`. It is not necessary to use
#'   `captureStackTraces` around the same expression as
#'   `withLogErrors`, as the latter includes a call to the former. Note
#'   that if `expr` contains calls (either directly or indirectly) to
#'   `try`, or `tryCatch` with an error handler, stack traces therein
#'   cannot be captured unless another `captureStackTraces` call is
#'   inserted in the interior of the `try` or `tryCatch`. This is
#'   because these calls catch the error and prevent it from traveling up to the
#'   condition handler installed by `captureStackTraces`.
#'
#' @param expr The expression to wrap.
#' @rdname stacktrace
#' @export
captureStackTraces <- function(expr) {
  promises::with_promise_domain(createStackTracePromiseDomain(),
    expr
  )
}

#' @include globals.R
.globals$deepStack <- NULL

createStackTracePromiseDomain <- function() {
  # These are actually stateless, we wouldn't have to create a new one each time
  # if we didn't want to. They're pretty cheap though.
  
  d <- promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      # Subscription time
      if (deepStacksEnabled()) {
        currentStack <- sys.calls()
        currentParents <- sys.parents()
        attr(currentStack, "parents") <- currentParents
        currentDeepStack <- .globals$deepStack
      }
      function(...) {
        # Fulfill time
        if (deepStacksEnabled()) {
          origDeepStack <- .globals$deepStack
          .globals$deepStack <- c(currentDeepStack, list(currentStack))
          on.exit(.globals$deepStack <- origDeepStack, add = TRUE)
        }

        withCallingHandlers(
          onFulfilled(...),
          error = doCaptureStack
        )
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      # Subscription time
      if (deepStacksEnabled()) {
        currentStack <- sys.calls()
        currentParents <- sys.parents()
        attr(currentStack, "parents") <- currentParents
        currentDeepStack <- .globals$deepStack
      }
      function(...) {
        # Fulfill time
        if (deepStacksEnabled()) {
          origDeepStack <- .globals$deepStack
          .globals$deepStack <- c(currentDeepStack, list(currentStack))
          on.exit(.globals$deepStack <- origDeepStack, add = TRUE)
        }

        withCallingHandlers(
          onRejected(...),
          error = doCaptureStack
        )
      }
    },
    wrapSync = function(expr) {
      withCallingHandlers(expr,
        error = doCaptureStack
      )
    },
    onError = doCaptureStack
  )
}

deepStacksEnabled <- function() {
  getOption("shiny.deepstacktrace", TRUE)
}

doCaptureStack <- function(e) {
  if (is.null(attr(e, "stack.trace", exact = TRUE))) {
    calls <- sys.calls()
    parents <- sys.parents()
    attr(calls, "parents") <- parents
    attr(e, "stack.trace") <- calls
  }
  if (deepStacksEnabled()) {
    if (is.null(attr(e, "deep.stack.trace", exact = TRUE)) && !is.null(.globals$deepStack)) {
      attr(e, "deep.stack.trace") <- .globals$deepStack
    }
  }
  stop(e)
}

#' @details `withLogErrors` captures stack traces and logs errors that
#'   occur in `expr`, but does allow errors to propagate beyond this point
#'   (i.e. it doesn't catch the error). The same caveats that apply to
#'   `captureStackTraces` with regard to `try`/`tryCatch` apply
#'   to `withLogErrors`.
#' @rdname stacktrace
#' @export
withLogErrors <- function(expr,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  withCallingHandlers(
    {
      result <- captureStackTraces(expr)

      # Handle expr being an async operation
      if (promises::is.promise(result)) {
        result <- promises::catch(result, function(cond) {
          # Don't print shiny.silent.error (i.e. validation errors)
          if (inherits(cond, "shiny.silent.error")) return()
          if (isTRUE(getOption("show.error.messages"))) {
            printError(cond, full = full, offset = offset)
          }
        })
      }

      result
    },
    error = function(cond) {
      # Don't print shiny.silent.error (i.e. validation errors)
      if (inherits(cond, "shiny.silent.error")) return()
      if (isTRUE(getOption("show.error.messages"))) {
        printError(cond, full = full, offset = offset)
      }
    }
  )
}

#' @details `printError` prints the error and stack trace (if any) using
#'   `warning(immediate.=TRUE)`. `printStackTrace` prints the stack
#'   trace only.
#'
#' @param cond An condition object (generally, an error).
#' @param full If `TRUE`, then every element of `sys.calls()` will be
#'   included in the stack trace. By default (`FALSE`), calls that Shiny
#'   deems uninteresting will be hidden.
#' @param offset If `TRUE` (the default), srcrefs will be reassigned from
#'   the calls they originated from, to the destinations of those calls. If
#'   you're used to stack traces from other languages, this feels more
#'   intuitive, as the definition of the function indicated in the call and the
#'   location specified by the srcref match up. If `FALSE`, srcrefs will be
#'   left alone (traditional R treatment where the srcref is of the callsite).
#' @rdname stacktrace
#' @export
printError <- function(cond,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {
  
  warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s", 
    getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
  
  printStackTrace(cond, full = full, offset = offset)
}

#' @rdname stacktrace
#' @export
printStackTrace <- function(cond,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  should_drop <- !full
  should_strip <- !full
  should_prune <- !full
  
  stackTraceCalls <- c(
    attr(cond, "deep.stack.trace", exact = TRUE),
    list(attr(cond, "stack.trace", exact = TRUE))
  )
  
  stackTraceParents <- lapply(stackTraceCalls, attr, which = "parents", exact = TRUE)
  stackTraceCallNames <- lapply(stackTraceCalls, getCallNames)
  stackTraceCalls <- lapply(stackTraceCalls, offsetSrcrefs, offset = offset)
  
  # Use dropTrivialFrames logic to remove trailing bits (.handleSimpleError, h)
  if (should_drop) {
    # toKeep is a list of logical vectors, of which elements (stack frames) to keep
    toKeep <- lapply(stackTraceCallNames, dropTrivialFrames)
    # We apply the list of logical vector indices to each data structure
    stackTraceCalls <- mapply(stackTraceCalls, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceCallNames <- mapply(stackTraceCallNames, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceParents <- mapply(stackTraceParents, FUN = `[`, toKeep, SIMPLIFY = FALSE)
  }
  
  delayedAssign("all_true", {
    # List of logical vectors that are all TRUE, the same shape as
    # stackTraceCallNames. Delay the evaluation so we don't create it unless
    # we need it, but if we need it twice then we don't pay to create it twice.
    lapply(stackTraceCallNames, function(st) {
      rep_len(TRUE, length(st))
    })
  })
  
  # stripStackTraces and lapply(stackTraceParents, pruneStackTrace) return lists
  # of logical vectors. Use mapply(FUN = `&`) to boolean-and each pair of the
  # logical vectors.
  toShow <- mapply(
    if (should_strip) stripStackTraces(stackTraceCallNames) else all_true,
    if (should_prune) lapply(stackTraceParents, pruneStackTrace) else all_true,
    FUN = `&`,
    SIMPLIFY = FALSE
  )
  
  dfs <- mapply(seq_along(stackTraceCalls), rev(stackTraceCalls), rev(stackTraceCallNames), rev(toShow), FUN = function(i, calls, nms, index) {
    st <- data.frame(
      num = rev(which(index)),
      call = rev(nms[index]),
      loc = rev(getLocs(calls[index])),
      category = rev(getCallCategories(calls[index])),
      stringsAsFactors = FALSE
    )
    
    if (i != 1) {
      message("From earlier call:")
    }

    if (nrow(st) == 0) {
      message("  [No stack trace available]")
    } else {
      width <- floor(log10(max(st$num))) + 1
      formatted <- paste0(
        "  ",
        formatC(st$num, width = width),
        ": ",
        mapply(paste0(st$call, st$loc), st$category, FUN = function(name, category) {
          if (category == "pkg")
            crayon::silver(name)
          else if (category == "user")
            crayon::blue$bold(name)
          else
            crayon::white(name)
        }),
        "\n"
      )
      cat(file = stderr(), formatted, sep = "")
    }

    st
  }, SIMPLIFY = FALSE)
  
  invisible()
}

#' @details `extractStackTrace` takes a list of calls (e.g. as returned
#'   from `conditionStackTrace(cond)`) and returns a data frame with one
#'   row for each stack frame and the columns `num` (stack frame number),
#'   `call` (a function name or similar), and `loc` (source file path
#'   and line number, if available). It was deprecated after shiny 1.0.5 because
#'   it doesn't support deep stack traces.
#' @rdname stacktrace
#' @export
extractStackTrace <- function(calls,
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {
  
  shinyDeprecated(NULL,
    "extractStackTrace is deprecated. Please contact the Shiny team if you were using this functionality.",
    version = "1.0.5")

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
    toShow <- (1 + cumsum(score)) > 0 & !(callnames %in% c("..stacktraceon..", "..stacktraceoff..", "..stacktracefloor.."))

    # doTryCatch, tryCatchOne, and tryCatchList are not informative--they're
    # just internals for tryCatch
    toShow <- toShow & !(callnames %in% c("doTryCatch", "tryCatchOne", "tryCatchList"))
  }
  calls <- calls[toShow]

  calls <- rev(calls) # Show in traceback() order
  index <- rev(which(toShow))
  width <- floor(log10(max(index))) + 1

  data.frame(
    num = index,
    call = getCallNames(calls),
    loc = getLocs(calls),
    category = getCallCategories(calls),
    stringsAsFactors = FALSE
  )
}

stripStackTraces <- function(stackTraces, values = FALSE) {
  score <- 1L  # >=1: show, <=0: hide
  lapply(seq_along(stackTraces), function(i) {
    res <- stripOneStackTrace(stackTraces[[i]], i != 1, score)
    score <<- res$score
    toShow <- as.logical(res$trace)
    if (values) {
      as.character(stackTraces[[i]][toShow])
    } else {
      as.logical(toShow)
    }
  })
}

stripOneStackTrace <- function(stackTrace, truncateFloor, startingScore) {
  prefix <- logical(0)
  if (truncateFloor) {
    indexOfFloor <- utils::tail(which(stackTrace == "..stacktracefloor.."), 1)
    if (length(indexOfFloor)) {
      stackTrace <- stackTrace[(indexOfFloor+1L):length(stackTrace)]
      prefix <- rep_len(FALSE, indexOfFloor)
    }
  }
  
  if (length(stackTrace) == 0) {
    return(list(score = startingScore, character(0)))
  }
  
  score <- rep.int(0L, length(stackTrace))
  score[stackTrace == "..stacktraceon.."] <- 1L
  score[stackTrace == "..stacktraceoff.."] <- -1L
  score <- startingScore + cumsum(score)
  
  toShow <- score > 0 & !(stackTrace %in% c("..stacktraceon..", "..stacktraceoff..", "..stacktracefloor.."))
  
  
  list(score = utils::tail(score, 1), trace = c(prefix, toShow))
}

# Given sys.parents() (which corresponds to sys.calls()), return a logical index
# that prunes each subtree so that only the final branch remains. The result,
# when applied to sys.calls(), is a linear list of calls without any "wrapper"
# functions like tryCatch, try, with, hybrid_chain, etc. While these are often
# part of the active call stack, they rarely are helpful when trying to identify
# a broken bit of code.
pruneStackTrace <- function(parents) {
  # Detect nodes that are not the last child. This is necessary, but not
  # sufficient; we also need to drop nodes that are the last child, but one of
  # their ancestors is not.
  is_dupe <- duplicated(parents, fromLast = TRUE)
  
  # The index of the most recently seen node that was actually kept instead of
  # dropped.
  current_node <- 0
  
  # Loop over the parent indices. Anything that is not parented by current_node
  # (a.k.a. last-known-good node), or is a dupe, can be discarded. Anything that
  # is kept becomes the new current_node.
  include <- vapply(seq_along(parents), function(i) {
    if (!is_dupe[[i]] && parents[[i]] == current_node) {
      current_node <<- i
      TRUE
    } else {
      FALSE
    }
  }, FUN.VALUE = logical(1))
  
  include
}

dropTrivialFrames <- function(callnames) {
  # Remove stop(), .handleSimpleError(), and h() calls from the end of
  # the calls--they don't add any helpful information. But only remove
  # the last *contiguous* block of them, and then, only if they are the
  # last thing in the calls list.
  hideable <- callnames %in% c(".handleSimpleError", "h", "base$wrapOnFulfilled")
  # What's the last that *didn't* match stop/.handleSimpleError/h?
  lastGoodCall <- max(which(!hideable))
  toRemove <- length(callnames) - lastGoodCall
  
  c(
    rep_len(TRUE, length(callnames) - toRemove),
    rep_len(FALSE, toRemove)
  )
}

offsetSrcrefs <- function(calls, offset = TRUE) {
  if (offset) {
    srcrefs <- getSrcRefs(calls)

    # Offset calls vs. srcrefs by 1 to make them more intuitive.
    # E.g. for "foo [bar.R:10]", line 10 of bar.R will be part of
    # the definition of foo().
    srcrefs <- c(utils::tail(srcrefs, -1), list(NULL))
    
    calls <- setSrcRefs(calls, srcrefs)
  }
  
  calls
}

#' @details `formatStackTrace` is similar to `extractStackTrace`, but
#'   it returns a preformatted character vector instead of a data frame. It was
#'   deprecated after shiny 1.0.5 because it doesn't support deep stack traces.
#' @param indent A string to prefix every line of the stack trace.
#' @rdname stacktrace
#' @export
formatStackTrace <- function(calls, indent = "    ",
  full = getOption("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  shinyDeprecated(NULL,
    "extractStackTrace is deprecated. Please contact the Shiny team if you were using this functionality.",
    version = "1.0.5")
  
  st <- extractStackTrace(calls, full = full, offset = offset)
  if (nrow(st) == 0) {
    return(character(0))
  }

  width <- floor(log10(max(st$num))) + 1
  paste0(
    indent,
    formatC(st$num, width = width),
    ": ",
    mapply(paste0(st$call, st$loc), st$category, FUN = function(name, category) {
      if (category == "pkg")
        crayon::silver(name)
      else if (category == "user")
        crayon::blue$bold(name)
      else
        crayon::white(name)
    })
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

#' @details `conditionStackTrace` and `conditionStackTrace<-` are
#'   accessor functions for getting/setting stack traces on conditions.
#'
#' @param cond A condition that may have previously been annotated by
#'   `captureStackTraces` (or `withLogErrors`).
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

#' @details The two functions `..stacktraceon..` and
#'   `..stacktraceoff..` have no runtime behavior during normal execution;
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

..stacktracefloor.. <- function(expr) expr
