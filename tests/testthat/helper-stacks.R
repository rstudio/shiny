#' @details `extractStackTrace` takes a list of calls (e.g. as returned
#'   from `conditionStackTrace(cond)`) and returns a data frame with one
#'   row for each stack frame and the columns `num` (stack frame number),
#'   `call` (a function name or similar), and `loc` (source file path
#'   and line number, if available). It was deprecated after shiny 1.0.5 because
#'   it doesn't support deep stack traces.
#' @rdname stacktrace
#' @export
extractStackTrace <- function(calls,
                              full = get_devmode_option("shiny.fullstacktrace", FALSE),
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
    toShow <- (1 + cumsum(score)) > 0 & !(callnames %in% c("..stacktraceon..", "..stacktraceoff..", "..stacktracefloor.."))

    toShow <-
      toShow &
      # doTryCatch, tryCatchOne, and tryCatchList are not informative--they're
      # just internals for tryCatch
      !(callnames %in% c("doTryCatch", "tryCatchOne", "tryCatchList")) &
      # doWithOneRestart and withOneRestart are not informative--they're
      # just internals for withRestarts
      !(callnames %in% c("withOneRestart", "doWithOneRestart"))
  }
  calls <- calls[toShow]


  calls <- rev(calls) # Show in traceback() order
  index <- rev(which(toShow))
  width <- floor(log10(max(index))) + 1

  data.frame(
    num = index,
    call = getCallNames(calls),
    loc = getLocs(calls),
    # category = getCallCategories(calls),
    stringsAsFactors = FALSE
  )
}

cleanLocs <- function(locs) {
  locs[!grepl("test-stacks\\.R", locs, perl = TRUE)] <- ""
  # sub("^.*#", "", locs)
  locs
}

dumpTests <- function(df) {
  print(bquote({
    expect_equal(df$num, .(df$num))
    expect_equal(df$call, .(df$call))
    expect_equal(nzchar(df$loc), .(nzchar(df$loc)))
  }))
}

# Helper: run a render function whose body throws an error, capture the
# stack trace, apply fence-based filtering, and return the filtered data
# frame. The render function body should call a function that calls stop().
# `needs_session` indicates whether the render function requires
# shinysession/name parameters (TRUE for markRenderFunction-based renders
# like renderPlot and renderPrint, FALSE for createRenderFunction-based
# renders like renderText/renderTable/renderUI/renderImage which can be
# called with no args).
captureFilteredRenderTrace <- function(render_fn, needs_session = TRUE) {
  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())

  res <- try({
      captureStackTraces({
        isolate({
          withReactiveDomain(session, {
            if (needs_session) {
              render_fn(shinysession = session, name = "testoutput")
            } else {
              render_fn()
            }
          })
        })
      })
    },
    silent = TRUE)

  cond <- attr(res, "condition", exact = TRUE)
  stopifnot(!is.null(cond))
  stopifnot(!is.null(conditionStackTrace(cond)))

  suppressMessages(
    extractStackTrace(conditionStackTrace(cond), full = FALSE)
  )
}
