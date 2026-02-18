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

# A stripped down version of getCallNames() that intentionally avoids deparsing expressions.
# Instead, it leaves expressions to be directly `rlang::hash()` (for de-duplication), which
# is much faster than deparsing then hashing.
getCallNamesForHash <- function(calls) {
  lapply(calls, function(call) {
    name <- call[[1L]]
    if (is.function(name)) return("<Anonymous>")
    if (typeof(name) == "promise") return("<Promise>")
    name
  })
}

# Get the preferred filename from a srcfile object.
#
# For user code, prefer the original path (as typed by user, potentially a
# symlink or relative path) over the normalized absolute path.
#
# For package files (under .libPaths()), keep the srcfile$filename because
# when a package is installed with keep.source.pkgs = TRUE, the original
# srcfilecopy filename may point to a collated build-time path rather than
# the real installed package path.
getSrcfileFilename <- function(srcfile) {
  if (!is.null(srcfile$original) &&
      !is.null(srcfile$original$filename) &&
      !isPackageFile(srcfile$filename)) {
    srcfile$original$filename
  } else {
    srcfile$filename
  }
}

# Get the source lines and correct line number from a srcfile + srcref.
#
# sourceUTF8() wraps user code with a `#line` directive that remaps line
# numbers. This means srcref[1] (the remapped line) may not correctly index
# into the srcfile's $lines. When a #line directive is present, R extends
# the srcref to 8 elements: [7] and [8] are the original (pre-remap) first
# and last line numbers in the srcfilecopy's coordinate system.
#
# Additionally, when the #line path differs from the srcfilecopy filename
# (e.g. macOS /tmp -> /private/tmp, or Windows path normalization), R wraps
# the srcfile in a srcfilealias whose $lines is NULL. In that case, we
# retrieve lines from the original srcfilecopy via $original.
getSrcfileLines <- function(srcfile, srcref) {
  lines <- srcfile$lines
  line_num <- srcref[1]

  if (is.null(lines) && inherits(srcfile, "srcfilealias")) {
    lines <- srcfile$original$lines
  }

  # Use the pre-remap line number when available and different from the
  # remapped line, indicating a #line directive shifted line numbering.
  if (length(srcref) >= 7 && srcref[7] != srcref[1]) {
    line_num <- srcref[7]
  }

  list(lines = lines, line_num = line_num)
}

getLocs <- function(calls) {
  vapply(calls, function(call) {
    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      if (!is.null(srcfile) && !is.null(srcfile$filename)) {
        loc <- paste0(getSrcfileFilename(srcfile), "#", srcref[[1]])
        return(paste0(" [", loc, "]"))
      }
    }
    return("")
  }, character(1))
}

# Check if a file path is in an R package library
isPackageFile <- function(filepath) {
  if (is.null(filepath) || filepath == "") {
    return(FALSE)
  }

  # Normalize paths for comparison
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
  lib_paths <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
  # Ensure trailing slash for proper path-boundary matching, otherwise
  # e.g. "/usr/lib/R" would incorrectly match "/usr/lib/Rcpp/..."
  lib_paths <- paste0(sub("/$", "", lib_paths), "/")

  # Check if the file is under any library path
  any(vapply(
    lib_paths,
    function(lib) identical(substr(filepath, 1, nchar(lib)), lib),
    logical(1)
  ))
}

getCallCategories <- function(calls) {
  vapply(calls, function(call) {
    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      if (!is.null(srcfile) && !is.null(srcfile$filename)) {
        # Use the absolute path for package detection (srcfile$filename)
        # rather than the original path which might be relative
        if (isPackageFile(srcfile$filename)) {
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
  # Use `promises::` as it shows up in the stack trace
  promises::with_promise_domain(
    createStackTracePromiseDomain(),
    expr
  )
}

#' @include globals.R
.globals$deepStack <- NULL

getCallStackDigest <- function(callStack, warn = FALSE) {
  dg <- attr(callStack, "shiny.stack.digest", exact = TRUE)
  if (!is.null(dg)) {
    return(dg)
  }

  if (isTRUE(warn)) {
    rlang::warn(
      "Call stack doesn't have a cached digest; expensively computing one now",
      .frequency = "once",
      .frequency_id = "deepstack-uncached-digest-warning"
    )
  }

  rlang::hash(getCallNamesForHash(callStack))
}

saveCallStackDigest <- function(callStack) {
  attr(callStack, "shiny.stack.digest") <- getCallStackDigest(callStack, warn = FALSE)
  callStack
}

# Appends a call stack to a list of call stacks, but only if it's not already
# in the list. The list is deduplicated by digest; ideally the digests on the
# list are cached before calling this function (you will get a warning if not).
appendCallStackWithDedupe <- function(lst, x) {
  digests <- vapply(lst, getCallStackDigest, character(1), warn = TRUE)
  xdigest <- getCallStackDigest(x, warn = TRUE)
  stopifnot(all(nzchar(digests)))
  stopifnot(length(xdigest) == 1)
  stopifnot(nzchar(xdigest))
  if (xdigest %in% digests) {
    return(lst)
  } else {
    return(c(lst, list(x)))
  }
}

createStackTracePromiseDomain <- function() {
  # These are actually stateless, we wouldn't have to create a new one each time
  # if we didn't want to. They're pretty cheap though.

  d <- new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      # Subscription time
      if (deepStacksEnabled()) {
        currentStack <- sys.calls()
        currentParents <- sys.parents()
        attr(currentStack, "parents") <- currentParents
        currentStack <- saveCallStackDigest(currentStack)
        currentDeepStack <- .globals$deepStack
      }
      function(...) {
        # Fulfill time
        if (deepStacksEnabled()) {
          origDeepStack <- .globals$deepStack
          .globals$deepStack <- appendCallStackWithDedupe(currentDeepStack, currentStack)
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
        currentStack <- saveCallStackDigest(currentStack)
        currentDeepStack <- .globals$deepStack
      }
      function(...) {
        # Fulfill time
        if (deepStacksEnabled()) {
          origDeepStack <- .globals$deepStack
          .globals$deepStack <- appendCallStackWithDedupe(currentDeepStack, currentStack)
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
    calls <- saveCallStackDigest(calls)
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
      if (is.promise(result)) {
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
  # interact. So we need to do this in one go, instead of individually within
  # printOneStackTrace.
  if (!full) {
    stripResults <- stripStackTraces(lapply(stackTraces, getCallNames))
  } else {
    # If full is TRUE, we don't want to strip anything
    stripResults <- rep_len(list(TRUE), length(stackTraces))
  }

  mapply(
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

  # If we're running in testthat, hide the parts of the stack trace that can
  # vary based on how testthat was launched. It's critical that this is not
  # happen at the same time as dropTrivialFrames, which happens before
  # pruneStackTrace; because dropTrivialTestFrames removes calls from the top
  # (or bottom? whichever is the oldest?) of the stack, it breaks `parents`
  # which is based on absolute indices of calls. dropTrivialFrames gets away
  # with this because it only removes calls from the opposite side of the stack.
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
          cli::col_silver(name)
        else if (category == "user")
          cli::style_bold(cli::col_blue(name))
        else
          cli::col_white(name)
      }),
      "\n"
    )
    cat(file = stderr(), formatted, sep = "")
  }

  invisible(st)
}

# Filter stack traces using fence markers to hide internal Shiny frames.
#
# `stackTraces` is a list of character vectors (call names), one per "segment".
# A single synchronous error produces one segment (the immediate call stack).
# Asynchronous errors (e.g. from promises) produce multiple segments: the deep
# stack trace segments come first, then the current segment last. Each deep
# segment may begin with frames that overlap the previous segment; a
# `..stacktracefloor..` marker delimits this redundant prefix from the active
# portion.
#
# Within the active frames, `..stacktraceon..` / `..stacktraceoff..` markers
# act as fences. Frames between a matched off/on pair (reading innermost to
# outermost) are hidden — these are the internal rendering pipeline frames
# that users don't need to see. The algorithm uses a *reverse clamped cumulative
# sum* so that an unmatched `..stacktraceoff..` (one with no corresponding
# inner `..stacktraceon..`) is a no-op, preventing it from hiding user frames.
# Fence matching works globally across segments so that a `..stacktraceoff..`
# at the end of one segment can pair with a `..stacktraceon..` at the start
# of the next.
stripStackTraces <- function(stackTraces, values = FALSE) {
  n_segs <- length(stackTraces)
  if (n_segs == 0L) return(list())

  # Replace NULL segments with empty character vectors
  stackTraces <- lapply(stackTraces, function(st) st %||% character(0))
  seg_lengths <- lengths(stackTraces)
  total <- sum(seg_lengths)

  if (total == 0L) {
    return(lapply(seg_lengths, function(n) {
      if (values) character(0) else logical(0)
    }))
  }

  # Pre-compute segment boundaries (used in steps 1 and 4)
  seg_ends <- cumsum(seg_lengths)
  seg_starts <- c(1L, seg_ends[-n_segs] + 1L)

  # Concatenate all segments into one vector for vectorized operations
  all <- unlist(stackTraces)

  # 1. Identify prefix elements (at/before last ..stacktracefloor.. in segs 2+)
  # Prefix elements are always hidden and excluded from fence scoring.
  is_active <- rep.int(TRUE, total)
  if (n_segs >= 2L) {
    for (i in 2:n_segs) {
      if (seg_lengths[i] == 0L) next
      seg_idx <- seg_starts[i]:seg_ends[i]
      floor_pos <- which(all[seg_idx] == "..stacktracefloor..")
      if (length(floor_pos)) {
        is_active[seg_idx[seq_len(floor_pos[length(floor_pos)])]] <- FALSE
      }
    }
  }

  # 2. Compute fence scores and marker mask (vectorized across all segments)
  is_on <- all == "..stacktraceon.."
  is_off <- all == "..stacktraceoff.."
  is_marker <- is_on | is_off | (all == "..stacktracefloor..")
  scores <- integer(total)
  scores[is_active & is_on] <- 1L
  scores[is_active & is_off] <- -1L

  # 3. Reverse clamped cumsum across all segments.
  # Process from innermost (right) to outermost (left). ..stacktraceon.. (+1)
  # opens a hidden region working outward, ..stacktraceoff.. (-1) closes it.
  # Clamping at 0 means an unmatched ..stacktraceoff.. (one with no inner
  # ..stacktraceon..) is a no-op. Prefix elements have score 0 and pass the
  # running total through unchanged.
  #
  # Vectorized via the identity: clamped_cumsum = cumsum - pmin(0, cummin(cumsum))
  rs <- rev(scores)
  cs <- cumsum(rs)
  depth <- rev(cs - pmin.int(0L, cummin(cs)))

  # 4. Compute visibility (vectorized) and split back into segments
  toShow <- is_active & depth == 0L & !is_marker

  lapply(seq_len(n_segs), function(i) {
    if (seg_lengths[i] == 0L) {
      if (values) return(character(0)) else return(logical(0))
    }
    idx <- seg_starts[i]:seg_ends[i]
    if (values) as.character(all[idx[toShow[idx]]]) else toShow[idx]
  })
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
  if (!identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")) {
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

  # Remove everything from inception to calling the test
  # It shouldn't matter how you get there, just that you're finally testing
  toRemove <- max(which(hideable))

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
