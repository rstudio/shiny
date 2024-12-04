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

# (Warning, this is pretty crazy)
#
# appendWithLimit lets us append an element to a list, but if the list starts to
# exceed a limit, it will start eliding elements from the middle of the list.
# At the point of elision, it will insert a placeholder integer that indicates
# how many elements were elided.
#
# This is useful for storing deep stack traces, where we don't want to overwhelm
# the user with a huge number of stack traces, but both the head and tail may
# contain important information.
#
# Example:
# lst <- list("a", "b", "c", "d")
#
# appendWithLimit(lst, "e", limit = 4)
# # Result: list("a", 1L, "c", "d", "e")
#
# appendWithLimit(lst, "e", limit = 4, retain_first_n = 2L)
# # Result: list("a", "b", 1L, "d", "e")
appendWithLimit <- function(lst, x, limit, retain_first_n = 1L) {
  # Sanity check that the parameters make sense
  stopifnot(retain_first_n > 0)
  stopifnot(limit > retain_first_n)
  stopifnot(!is.null(x))
  stopifnot(is.null(lst) || is.list(lst))

  elide_count_index <- which(vapply(lst, is.integer, logical(1)))
  elide_count <- sum(as.integer(lst[elide_count_index]))

  # Sanity check -- shouldn't have more than one elide placeholder
  stopifnot(length(elide_count_index) <= 1L)
  if (length(elide_count_index) == 1L) {
    # For a given list, retain_first_n must remain constant between calls to
    # appendWithLimit; that is, the elide count, if one exists, must be exactly
    # where we expect it to be
    stopifnot(elide_count_index == retain_first_n + 1L)
  }

  element_count <- length(lst) - length(elide_count_index)
  if (element_count < limit) {
    # No need to elide anything, just append and return
    c(lst, list(x))
  } else {
    # The list may or may not already contain an elide count. If it does, we
    # need to increment it. If it doesn't, we need to add one.

    # Keep the first `retain_first_n` elements...
    prefix <- utils::head(lst, retain_first_n)
    # ...and just enough of the last elements to leave room for the new one
    suffix <- utils::tail(lst, limit - retain_first_n - 1L)
    # Calculate the new elide count
    new_elide_count <- (element_count - limit + 1L) + elide_count
    # Construct the final list
    c(prefix, list(as.integer(new_elide_count)), suffix, list(x))
  }
}

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
          .globals$deepStack <- appendWithLimit(currentDeepStack, currentStack, deepStackLimit())
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
          .globals$deepStack <- appendWithLimit(currentDeepStack, currentStack, deepStackLimit())
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
  deepStackLimit() > 0L
}

deepStackLimit <- function() {
  opt <- getOption("shiny.deepstacktrace", 8L)
  if (!is.numeric(opt) && !is.logical(opt)) {
    opt <- FALSE
  }
  stopifnot(length(opt) == 1L)

  if (rlang::is_false(opt)) {
    0L
  } else if (isTRUE(opt)) {
    Inf
  } else {
    as.integer(opt)
  }
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
  full = get_devmode_option("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  withCallingHandlers(
    {
      result <- captureStackTraces(expr)

      # Handle expr being an async operation
      if (promises::is.promise(result)) {
        result <- promises::catch(result, function(cond) {
          # Don't print shiny.silent.error (i.e. validation errors)
          if (cnd_inherits(cond, "shiny.silent.error")) {
            return()
          }
          if (isTRUE(getOption("show.error.messages"))) {
            printError(cond, full = full, offset = offset)
          }
        })
      }

      result
    },
    error = function(cond) {
      # Don't print shiny.silent.error (i.e. validation errors)
      if (cnd_inherits(cond, "shiny.silent.error")) return()
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
  full = get_devmode_option("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s",
    getCallNames(list(conditionCall(cond))), conditionMessage(cond)))

  printStackTrace(cond, full = full, offset = offset)
}

#' @rdname stacktrace
#' @export
printStackTrace <- function(cond,
  full = get_devmode_option("shiny.fullstacktrace", FALSE),
  offset = getOption("shiny.stacktraceoffset", TRUE)) {

  stackTraces <- c(
    attr(cond, "deep.stack.trace", exact = TRUE),
    list(attr(cond, "stack.trace", exact = TRUE))
  )

  # Stripping of stack traces is the one step where the different stack traces
  # interact.
  stripResults <- stripStackTraces(lapply(stackTraces, function(trace) {
    if (is.integer(trace)) {
      # This is where deep stacks were elided.
      # jcheng 2024-12-03: For the purposes of stack trace stripping, it's not
      # ideal that we're eliding entire stacks before we get here--this actually
      # potentially screws up our scoring (i.e., an unbalanced ..stacktraceon..
      # or ..stacktraceoff.. was elided). If this surfaces as an actual problem,
      # we could maybe maintain a cumulative score of the stacks as we elide
      # them.
      character(0)
    } else {
      getCallNames(trace)
    }
  }))

  dfs <- mapply(
    seq_along(stackTraces),
    rev(stackTraces),
    rev(stripResults),
    FUN = function(i, trace, stripResult) {
      if (is.integer(trace)) {
        noun <- if (trace > 1L) "traces" else "trace"
        message("[ reached getOption(\"shiny.deepstacktrace\") -- omitted ", trace, " more stack ", noun, " ]")
      } else {
        if (i != 1) {
          message("From earlier call:")
        }
        printOneStackTrace(
          stackTrace = trace,
          stripResult = stripResult,
          full = full,
          offset = offset
        )
      }
      # No mapply return value--we're just printing
      NULL
    },
    SIMPLIFY = FALSE
  )

  invisible()
}

printOneStackTrace <- function(stackTrace, stripResult, full, offset) {
  calls <- offsetSrcrefs(stackTrace, offset = offset)
  callNames <- getCallNames(stackTrace)
  parents <- attr(stackTrace, "parents", exact = TRUE)

  should_drop <- !full
  should_strip <- !full
  should_prune <- !full

  if (should_drop) {
    toKeep <- dropTrivialFrames(callNames)
    calls <- calls[toKeep]
    callNames <- callNames[toKeep]
    parents <- parents[toKeep]
    stripResult <- stripResult[toKeep]
  }

  toShow <- rep(TRUE, length(callNames))
  if (should_prune) {
    toShow <- toShow & pruneStackTrace(parents)
  }
  if (should_strip) {
    toShow <- toShow & stripResult
  }

  # If we're running in testthat, hide the parts of
  toShow <- toShow & dropTrivialTestFrames(callNames)

  st <- data.frame(
    num = rev(which(toShow)),
    call = rev(callNames[toShow]),
    loc = rev(getLocs(calls[toShow])),
    category = rev(getCallCategories(calls[toShow])),
    stringsAsFactors = FALSE
  )

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

  invisible(st)
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
  #
  # jcheng 2022-03-18: Two more reasons a node can be kept:
  #   1. parent is 0
  #   2. parent is i
  # Not sure why either of these situations happen, but they're common when
  # interacting with rlang/dplyr errors. See issue rstudio/shiny#3250 for repro
  # cases.
  include <- vapply(seq_along(parents), function(i) {
    if ((!is_dupe[[i]] && parents[[i]] == current_node) ||
        parents[[i]] == 0 ||
        parents[[i]] == i) {
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

dropTrivialTestFrames <- function(callnames) {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    return(rep_len(TRUE, length(callnames)))
  }

  hideable <- callnames %in% c(
    "test",
    "devtools::test",
    "test_check",
    "testthat::test_check",
    "test_dir",
    "testthat::test_dir",
    "test_file",
    "testthat::test_file",
    "test_local",
    "testthat::test_local"
  )

  firstGoodCall <- min(which(!hideable))
  toRemove <- firstGoodCall - 1L

  c(
    rep_len(FALSE, toRemove),
    rep_len(TRUE, length(callnames) - toRemove)
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
