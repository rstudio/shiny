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
  if (isTRUE(length(srcref) >= 7 && srcref[7] != srcref[1])) {
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
    # Stacks saved at different `await()`s of the same code have the same
    # digest. Keep the latest, as it has the line of the latest `await()`.
    if (!is.null(attr(x, "asyncFrames", exact = TRUE))) {
      lst[[match(xdigest, digests)]] <- x
    }
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
  stop(withDeepStackTrace(e))
}

withDeepStackTrace <- function(e) {
  if (
    deepStacksEnabled() &&
      is.null(attr(e, "deep.stack.trace", exact = TRUE)) &&
      !is.null(.globals$deepStack)
  ) {
    attr(e, "deep.stack.trace") <- .globals$deepStack
  }
  e
}

# Like `doCaptureStack()`, for errors that pass through code using `await()`.
# It is the error handler of the two places that run such code: the wrapper
# made by `wrapFunctionLabel()` (up to the first `await()`) and the callback
# made by `asyncCallback()` (after each `await()`). So the code itself says
# that its stack needs `relabelAsyncFrames()`; nothing outside it has to know
# or check, whichever promise domain or session runs next.
#
# A handler nearer the error may have saved the stack already, as when the
# code reads a reactive that fails. That stack is relabelled too, if its
# frames are still running. An error that brought its stack from elsewhere
# (such as the reason of a rejected promise) is left alone.
doCaptureAsyncStack <- function(e) {
  calls <- attr(e, "stack.trace", exact = TRUE)
  if (is.null(calls)) {
    calls <- sys.calls()
    parents <- sys.parents()
    frames <- sys.frames()
  } else if (
    is.null(attr(calls, "asyncFrames", exact = TRUE)) && isRunningStack(calls)
  ) {
    parents <- attr(calls, "parents", exact = TRUE)
    frames <- sys.frames()[seq_along(calls)]
  } else {
    return()
  }
  calls <- relabelAsyncFrames(calls, parents, frames)
  label <- attr(calls, "asyncLabel", exact = TRUE)
  # coro evaluates the user's code with `eval_bare()`, which tells the user
  # nothing; use the reactive's label, as for code without `await()`
  if (!is.null(label) && isCoroEvalCall(conditionCall(e))) {
    e$call <- call(label)
  }
  attr(calls, "parents") <- parents
  # The names changed, so the digest does too
  attr(calls, "shiny.stack.digest") <- NULL
  calls <- saveCallStackDigest(calls)
  attr(e, "stack.trace") <- calls
  stop(withDeepStackTrace(e))
}

# Whether `calls` was saved by a `doCaptureStack()` that is still on the
# stack: it is then the `calls` of the frame at its last index, and the frames
# before it are the ones it lists.
isRunningStack <- function(calls) {
  n <- length(calls)
  n > 0 && n < sys.nframe() && identical(sys.frame(n)$calls, calls)
}

# The callback for coro to run when the promise of an `await()` resolves; used
# in place of coro's own `callback` (see `shimAsyncOps()`). It carries what the
# code after the `await()` needs for its stack traces, so that is found
# without asking what else is running:
# - the stack at the `await()`, relabelled, for the deep stack trace. A stack
#   trace promise domain saves that stack too, but as it is; this one takes
#   its place.
# - `doCaptureAsyncStack()` as the error handler.
# `value` is not forced here: for a rejected promise, coro passes a `stop()`
# to raise inside the user's code.
asyncCallback <- function(callback) {
  force(callback)
  deep <- deepStacksEnabled()
  if (deep) {
    parents <- sys.parents()
    stack <- relabelAsyncFrames(sys.calls(), parents, sys.frames(), atThen = TRUE)
    attr(stack, "parents") <- parents
    stack <- saveCallStackDigest(stack)
    deepStack <- .globals$deepStack
  }
  function(value) {
    if (deep) {
      origDeepStack <- .globals$deepStack
      .globals$deepStack <- appendCallStackWithDedupe(deepStack, stack)
      on.exit(.globals$deepStack <- origDeepStack, add = TRUE)
    }
    withCallingHandlers(
      callWithoutCompiling(callback, value),
      error = doCaptureAsyncStack
    )
  }
}

# Call `fun(arg)` without letting R's JIT compile `fun`.
# coro makes a new function, with a new body, for each run of code using
# `await()`. R compiles it the first time it is resumed, which takes far longer
# (10 ms or more) than running it, and the result is used for that one run.
# The JIT is only off while `fun` is entered: `arg` is lazy, and coro reads it
# just before it carries on with the user's code, which turns the JIT back on.
callWithoutCompiling <- function(fun, arg) {
  jit <- compiler::enableJIT(0L)
  on.exit(compiler::enableJIT(jit), add = TRUE)
  fun({
    compiler::enableJIT(jit)
    arg
  })
}

# Code that uses `await()` runs as a `coro::async()` function (see
# `awaitToAsyncFunction()`). coro evaluates the user's code in a frame of its
# own (an `evalq()` of `user_env`), under several more of its frames. Name
# that frame after the reactive, as `wrapFunctionLabel()` does for code
# without `await()`, marked as async (`renderText (coro::async)`), and mark
# coro's frames around it as hidden:
# - those between it and the user's code, up to coro's `eval_bare()`;
# - before the first `await()`, those between it and the call to the async
#   function (which it replaces);
# - after an `await()`, everything outside it, which is promise and event loop
#   plumbing. The deep stack trace shows where the code started.
# No other call is replaced, so a full stack trace still shows every frame.
# `atThen` is TRUE when the stack is saved at a `then()` rather than an error.
# `getFunction(j)` is the function of frame `j`; replaced in tests.
#
# When the stack has such a frame, these attrs are set on the result:
# - "asyncLabel": the label of the innermost one;
# - "asyncFrames": the indexes of the renamed frames;
# - "asyncHidden": a logical vector, TRUE for the frames to hide. They are
#   hidden by `stripStackTraces()`, which also leaves them out of fence
#   matching, so fences inside a hidden region can't hide anything else.
relabelAsyncFrames <- function(
  calls,
  parents,
  frames,
  atThen = FALSE,
  getFunction = sys.function
) {
  innermost <- NULL
  asyncFrames <- integer()
  hidden <- rep(FALSE, length(calls))
  # Found when first needed
  outermost <- NULL
  # Frames whose env's parent is an async function's env. Besides the frame
  # running the user's code, coro's own setup frames can match, so keep only
  # the innermost match for each async function.
  # This runs for every frame, so it is kept cheap: `$` does not look in
  # parent envs, and `sys.frames()` is a pairlist, which is slow to index.
  labelEnvs <- vector("list", length(frames))
  i <- 0L
  for (frame in frames) {
    i <- i + 1L
    env <- parent.env(frame)
    if (!is.null(env$.shinyAsyncLabel)) {
      labelEnvs[[i]] <- env
    }
  }
  matched <- which(lengths(labelEnvs) > 0L)
  if (!length(matched)) {
    return(calls)
  }
  # `sys.calls()` is a pairlist too; read the calls from a list
  callList <- as.list(calls)
  for (i in matched) {
    later <- labelEnvs[matched[matched > i]]
    if (any(vapply(later, identical, logical(1), labelEnvs[[i]]))) {
      next
    }
    label <- labelEnvs[[i]]$.shinyAsyncLabel
    innermost <- label
    asyncFrames <- c(asyncFrames, i)
    callList[[i]] <- structure(
      call(paste0(label, " (coro::async)")),
      srcref = attr(callList[[i]], "srcref", exact = TRUE)
    )
    calls[[i]] <- callList[[i]]
    # coro's frames between the user's frame and the user's code
    evalFrame <- coroEvalFrame(callList, i)
    if (!is.null(evalFrame)) {
      hidden[(i + 1):evalFrame] <- TRUE
    }
    parent <- parents[[i]]
    if (parent <= 0 || parent >= i) {
      next
    }
    # Hide from the call to the async function, if this is before the first
    # `await()`; otherwise from the outermost frame.
    # Scan back rather than follow `parents`: a frame can list itself (or a
    # later frame) as its parent, which breaks the chain. Check the function
    # too, not only the name: labels such as `observe` are shared, and after
    # an `await()` another frame can have the same one (when the event loop is
    # run from inside other reactive code).
    if (is.null(outermost)) {
      # Frames at or before the last `..stacktracefloor..` are handled by
      # `stripStackTraces()`, so the outermost frame to hide is the one just
      # inside it
      outermost <- 1L
      for (j in rev(seq_along(callList))) {
        if (identical(callList[[j]][[1]], stackTraceFloorSym)) {
          outermost <- j + 1L
          break
        }
      }
    }
    first <- outermost
    for (j in rev(seq_len(parent))) {
      if (
        isCallNamed(callList[[j]], label) &&
          isAsyncFunctionFor(getFunction(j), labelEnvs[[i]])
      ) {
        first <- j
        break
      }
    }
    # The wrapper's error handler is around that call
    if (first > 1L && isCallNamed(callList[[first - 1L]], "withCallingHandlers")) {
      first <- first - 1L
    }
    if (first <= parent) {
      hidden[first:parent] <- TRUE
    }
  }
  # When the stack is saved at a `then()` (for the deep stack trace), and the
  # `then()` is coro's at an `await()`, the frames inside the user's are
  # coro's and the promise package's; hide them
  n <- length(calls)
  innermostIndex <- max(asyncFrames)
  if (
    atThen &&
      innermostIndex < n &&
      identical(callList[[innermostIndex + 1]][[1]], quote(base::evalq))
  ) {
    inner <- (innermostIndex + 1):n
    hidden[inner] <- TRUE
    # Give the user's frame the line of the `await()`. A frame is shown with
    # the srcref of the call after it (see `offsetSrcrefs()`). If coro's
    # `then()` call is not found, or is not as `coroAwaitSrcref()` expects,
    # the frame is left with no line.
    for (k in inner) {
      if (isCallNamed(callList[[k]], "then")) {
        srcref <- coroAwaitSrcref(callList[[k]])
        if (!is.null(srcref)) {
          attr(calls[[innermostIndex + 1]], "srcref") <- srcref
        }
        break
      }
    }
  }
  attr(calls, "asyncLabel") <- innermost
  attr(calls, "asyncFrames") <- asyncFrames
  attr(calls, "asyncHidden") <- hidden
  calls
}

stackTraceFloorSym <- quote(..stacktracefloor..)

# Whether `fun` is the async function whose label is kept in `labelEnv`
isAsyncFunctionFor <- function(fun, labelEnv) {
  inherits(fun, "coro_async") &&
    identical(environment(coroSourceFn(fun)), labelEnv)
}

isCallNamed <- function(call, name) {
  head <- if (is.call(call)) call[[1]]
  (is.symbol(head) || is.character(head)) && identical(as.character(head), name)
}

isCoroEvalCall <- function(call) {
  is.call(call) && identical(call[[1]], quote(eval_bare))
}

# The srcref of the user's `await()` statement, given coro's call to `then()`
# for it: `then(as_promise(user({ <code up to the await> })), callback = .self)`.
# The block keeps the srcrefs of the user's statements; the last is the
# `await()`.
# Relies on coro internals: the shape of that call. If coro changes it, this
# returns NULL instead of failing, so the frame is shown without a file and
# line and the rest of the stack trace is unchanged. The "deep stack trace
# shows the file and line of each await()" test in test-stacks.R fails if that
# happens.
coroAwaitSrcref <- function(call) {
  block <- tryCatch(call[[2]][[2]][[2]], error = function(e) NULL)
  if (!is.call(block) || !identical(block[[1]], quote(`{`))) {
    return(NULL)
  }
  srcrefs <- attr(block, "srcref", exact = TRUE)
  if (length(srcrefs)) srcrefs[[length(srcrefs)]]
}

# The index of coro's `eval_bare()` frame that runs the user's code for the
# frame at index `i` (see `relabelAsyncFrames()`): the first one after it.
# NULL if there is none, as when the stack is saved at an `await()`.
coroEvalFrame <- function(calls, i) {
  for (k in seq_along(calls)[-seq_len(i)]) {
    if (isCoroEvalCall(calls[[k]])) {
      return(k)
    }
  }
  NULL
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
    stripResults <- stripStackTraces(
      lapply(stackTraces, getCallNames),
      hidden = lapply(stackTraces, attr, "asyncHidden", exact = TRUE)
    )
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
#
# `hidden` is an optional list with a logical vector (or NULL) per segment,
# TRUE for frames that are always hidden and, like the prefix, take no part in
# fence matching (see `relabelAsyncFrames()`).
stripStackTraces <- function(stackTraces, values = FALSE, hidden = NULL) {
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
  # The index (in `all`) of each segment's last floor; NA if it has none
  seg_floors <- rep.int(NA_integer_, n_segs)
  if (n_segs >= 2L) {
    for (i in 2:n_segs) {
      if (seg_lengths[i] == 0L) next
      seg_idx <- seg_starts[i]:seg_ends[i]
      floor_pos <- which(all[seg_idx] == "..stacktracefloor..")
      if (length(floor_pos)) {
        is_active[seg_idx[seq_len(floor_pos[length(floor_pos)])]] <- FALSE
        seg_floors[i] <- seg_idx[floor_pos[length(floor_pos)]]
      }
    }
  }

  for (i in seq_along(hidden)) {
    if (seg_lengths[i] > 0L && length(hidden[[i]]) == seg_lengths[i]) {
      is_active[(seg_starts[i]:seg_ends[i])[hidden[[i]]]] <- FALSE
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
  #
  # A hidden region that is still open at the outer end of a segment carries
  # on into the segment before it. That is meant for a callback that starts
  # with `..stacktraceon..`. It is not meant for fences of the code that ran
  # the event loop, when the loop is run from inside reactive code (such as a
  # `later::run_now()` in an observer): those are matched by a
  # `..stacktraceoff..` in the prefix, which is not scored. Left alone, they
  # would hide all the earlier segments. So cancel them at the floor, which
  # is hidden anyway.
  # Relies on later internals: `execCallbacks()` is the frame that runs the
  # event loop's callbacks. If it is renamed, nothing is cancelled, which is
  # how this worked before.
  for (i in which(!is.na(seg_floors))) {
    loop_pos <- which(all[seg_floors[i]:seg_ends[i]] == "execCallbacks")
    if (length(loop_pos) && max(loop_pos) > 1L) {
      outside_loop <- seg_floors[i] + seq_len(max(loop_pos) - 1L)
      open <- reverseClampedCumsum(scores[outside_loop])[[1L]]
      scores[seg_floors[i]] <- -open
    }
  }
  depth <- reverseClampedCumsum(scores)

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

# The running total of `scores` from the last element to the first, never
# going below 0
reverseClampedCumsum <- function(scores) {
  cs <- cumsum(rev(scores))
  rev(cs - pmin.int(0L, cummin(cs)))
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

    # The frame running the user's `await()` code (see `relabelAsyncFrames()`)
    # is not followed by the call it makes, but by coro's own frames, the last
    # of which is `eval_bare()`. So use the srcref of the call after that.
    # Looking for the latest call whose parent is the user's frame is not
    # enough: a lazily forced argument has such a parent, but the srcref of
    # the code that forced it.
    asyncFrames <- attr(calls, "asyncFrames", exact = TRUE)
    if (length(asyncFrames)) {
      original <- getSrcRefs(calls)
      for (i in asyncFrames) {
        evalFrame <- coroEvalFrame(calls, i)
        if (!is.null(evalFrame) && evalFrame < length(calls)) {
          srcrefs[i] <- list(original[[evalFrame + 1]])
        }
      }
    }

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
