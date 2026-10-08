#' @include utils.R
NULL

#' Resume sessions after a dropped connection
#'
#' @description
#' With resume on, Shiny saves each session's state as it goes, and a page
#' whose connection drops reconnects and continues from it, whether the R
#' process survived or was restarted: [reactiveVal()] and [reactiveValues()]
#' values, `input`, uploaded files and `session$userData` come back;
#' [observeEvent()] handlers whose events already happened do not fire again;
#' [reactive()] expressions and outputs recompute. Inputs that changed while
#' the page was disconnected are applied on top.
#'
#' `enableResume()` turns resume on for an app and configures it;
#' `disableResume()` turns it off. Call either at the top level of `app.R`
#' (before [shinyApp()]) or in `global.R`, not in `server.R` or inside the
#' server function. Like [enableBookmarking()], the call applies to the app
#' created next; when an app calls both, the last call wins. Resume implies
#' `session$allowReconnect(TRUE)` for the app's sessions; a session may still
#' call `session$allowReconnect(FALSE)` to opt out, in which case it is
#' neither retried nor resumed.
#'
#' `disableResume(x)` leaves one reactive out:
#'
#' * On a [reactiveVal()] or [reactiveValues()] object: its value is not
#'   saved and does not count against the all-or-nothing rule below; after a
#'   resume it holds its initial value.
#' * On an [ExtendedTask]: its state is not saved; after a resume the task is
#'   as if never invoked.
#' * On an observer made with [observeEvent()] or [bindEvent()]: its first
#'   run is not held back; it runs on resume exactly as on a fresh start,
#'   `ignoreInit` included. Use it on a handler whose job is to build
#'   something. A plain [observe()] always runs on resume, so marking one
#'   changes nothing.
#'
#' @section What resume does not do:
#' * **A page reload starts fresh**, as without resume. Resume is for a page
#'   that is still open.
#' * **An unhandled error in an observer ends the session**, and its saved
#'   state is deleted. So does `session$close()`. The page is not retried.
#' * Structure an `observeEvent()` handler created after startup (an output,
#'   a module server, an observer) does not come back, because that handler
#'   does not run again. See rule 3.
#' * Closure state (`<<-`), reference objects and async work in flight are
#'   never saved.
#'
#' @section Who decides:
#' Resolved once per app, first match wins:
#'
#' 1. The app's last `enableResume()` or `disableResume()` call.
#' 2. The `SHINY_RESUME` environment variable (`TRUE` or `FALSE`), for a
#'    hosting platform that has verified its storage.
#' 3. Off.
#'
#' @section What is saved:
#' Every `reactiveVal()` and `reactiveValues()` with a label: an explicit
#' `label` (`.label` for `reactiveValues()`), or the name it is assigned to
#' (`count <- reactiveVal(0)`), in app code and in installed packages alike.
#' A source with no label (one created in `lapply()`, or by a helper
#' function) is not saved and after a resume holds its initial value; in
#' development mode ([devmode()]) Shiny names it. A value that cannot be
#' serialized (a connection, an R6 object, a closure), one over
#' `maxNodeSize`, or two sources sharing a label block saved state for the
#' whole session: it is restored all or nothing, so the session resumes
#' from its inputs only and the log says why. `session$resumeReport()`
#' lists every value and input with the reason it would not restore.
#'
#' Values that commonly block resume because they carry an environment or a
#' function: model fits and formulas created inside a function, ggplot
#' objects, S7 objects, and anything holding a connection, an external
#' pointer or an R6 object. Keep them in a [reactive()], which recomputes
#' after a resume, or mark the source with `disableResume()`.
#'
#' @section The rules of reactives:
#' Four habits keep resumed sessions complete and correct. In development
#' mode Shiny names the rule a session breaks.
#'
#' 1. **Label sources created in loops or through helpers.** A source made
#'    in `lapply()` or by a wrapper function has no name to infer. Give it
#'    one (`reactiveVal(0, label = paste0("count", i))`) or mark it with
#'    `disableResume()`.
#' 2. **Hold reference objects in a `reactive()`, not a `reactiveVal()`.** A
#'    connection, pool or R6 client cannot be saved; in a `reactive()` it is
#'    rebuilt on first read. If one must live in a source, mark both the
#'    source and the observer that builds it with `disableResume()`.
#' 3. **Build structure from state, not in handlers.** A handler that
#'    defines an output, starts a module server or creates an observer does
#'    not run again after a resume, so what it built is missing; the log
#'    names outputs on the page that have no definition. Keep the ids in a
#'    `reactiveVal()`, render the UI with [renderUI()], and start module
#'    servers from a plain [observe()] over the ids; or mark the handler with
#'    `disableResume()` if it builds exactly one thing and has no other side
#'    effect.
#' 4. **Accumulate into a reactive value from `observeEvent()`, not a plain
#'    `observe()`.** A plain observer has no event to hold back, so it runs
#'    on resume and applies its change again:
#'    `observe({ input$plus; isolate(count(count() + 1)) })` shows 4 after
#'    three clicks and a resume; `observeEvent(input$plus, count(count() + 1))`
#'    shows 3.
#'
#' Also: [bindCache()] with a persistent cache keeps expensive reactive
#' expressions cheap after a restart; [eventReactive()] recomputes with the
#' current inputs; `observeEvent(once = TRUE)` may fire once more after a
#' resume; process-scoped values (temp paths, process ids) serialize fine
#' and are wrong after a restart; an unconditional top-level
#' `session$userData$x <- 0` overwrites the saved value, so guard it with
#' `if (is.null(session$userData$x))` or use a `reactiveVal()`.
#'
#' @section What the log says:
#' Each resume writes one line to standard error: `Resumed: restored N
#' values and M inputs; K event handlers skipped, J re-ran`, or `Resumed from
#' inputs only: <cause>`, plus `Outputs on the page with no definition after
#' resume: ...` when rule 3 is broken.
#'
#' @section Where saved state lives:
#' In the hosting platform's bookmark storage when it provides one, otherwise
#' under `tools::R_user_dir("shiny", "cache")`; never in the app directory.
#' Records are readable only by the owner, keyed by a 128-bit random token
#' and the app's path, kept for 24 hours, deleted when the page is closed or
#' the session ends for good, and the directory as a whole is capped at
#' 1 GiB and 1000 records. On Windows the token comes from the `openssl`
#' package when it is installed and from R's random number generator
#' otherwise. Whether a hosting platform's directory survives a restart of
#' the R process is up to the platform and has not yet been verified for
#' Posit's hosted products; a container without a persistent volume resumes
#' from inputs only after it restarts.
#'
#' @param appVersion `NULL`, or a string or number recorded with the saved
#'   state. Change it in a deploy that changes what the app's state means, so
#'   sessions saved by the previous version resume from inputs only.
#' @param maxSize Bytes; the total size of a session's saved state. Over it,
#'   the largest values block resume until the rest fit.
#' @param maxNodeSize Bytes; a value whose serialized size exceeds this
#'   blocks resume.
#' @param debounce Milliseconds of quiet after a change before the state is
#'   saved. While changes keep arriving it is saved at least every ten such
#'   intervals.
#' @param x Optional. A reactive value, `reactiveValues()` object, observer
#'   or `ExtendedTask` to leave out of resume. Without it, `disableResume()`
#'   turns resume off for the app.
#' @return `enableResume()` and `disableResume()` return `NULL` invisibly;
#'   `disableResume(x)` returns `x` invisibly.
#' @seealso [session] for `session$resumeReport()` and
#'   `session$allowReconnect()`; [reactiveValues()] for `.label`.
#' @examples
#' \dontrun{
#' # app.R
#' library(shiny)
#' enableResume(appVersion = "2")
#'
#' ui <- fluidPage(actionButton("plus", "+1"), textOutput("count"))
#' server <- function(input, output, session) {
#'   count <- reactiveVal(0)
#'   observeEvent(input$plus, count(count() + 1))
#'
#'   # A connection cannot be saved: keep it in a reactive(), which recomputes.
#'   conn <- reactive(file(tempfile(), open = "w"))
#'
#'   output$count <- renderText(count())
#' }
#' shinyApp(ui, server)
#' }
#' @export
enableResume <- function(appVersion = NULL, maxSize = 50e6, maxNodeSize = 5e6, debounce = 1000) {
  resume_check_top_level("enableResume")
  settings <- list(
    enabled = TRUE,
    appVersion = resume_check_version(appVersion),
    maxSize = resume_check_number(maxSize, "maxSize", "a single positive number of bytes", positive = TRUE),
    maxNodeSize = resume_check_number(maxNodeSize, "maxNodeSize", "a single positive number of bytes", positive = TRUE),
    debounce = resume_check_number(debounce, "debounce", "a single non-negative number of milliseconds", positive = FALSE)
  )
  shinyOptions(resume = settings)
  invisible()
}

#' @rdname enableResume
#' @export
disableResume <- function(x) {
  if (missing(x)) {
    resume_check_top_level("disableResume")
    shinyOptions(resume = c(list(enabled = FALSE), resume_defaults()))
    return(invisible())
  }
  UseMethod("disableResume")
}

#' @export
disableResume.default <- function(x) {
  rlang::abort(paste0(
    "disableResume() expects a reactiveVal, reactiveValues object, observer, or ExtendedTask, not an object of class ",
    paste(class(x), collapse = "/"), "."
  ))
}

resume_check_top_level <- function(fn) {
  # server.R is sourced when the first session starts, after that session
  # resolved its settings; a call there would reach later sessions only.
  if (isTRUE(.globals$sourcingServerR)) {
    rlang::abort(sprintf("Call %s() in global.R, not server.R: server.R runs only once the first session has started.", fn))
  }
  if (is.null(getDefaultReactiveDomain())) return(invisible())
  rlang::abort(switch(fn,
    enableResume = "Call enableResume() at the top level of the app (in app.R before shinyApp(), or in global.R), not inside a session.",
    disableResume = "Call disableResume() at the top level of the app to turn resume off, or pass it a reactive."
  ))
}

resume_check_number <- function(value, arg, what, positive) {
  ok <- is.numeric(value) && length(value) == 1 && !is.na(value) &&
    (if (positive) value > 0 else value >= 0)
  if (!ok) rlang::abort(sprintf("`%s` must be %s.", arg, what))
  as.numeric(value)
}

resume_check_version <- function(value) {
  if (is.null(value)) return(NULL)
  if ((is.character(value) || is.numeric(value)) && length(value) == 1 && !is.na(value)) {
    return(as.character(value))
  }
  rlang::abort("`appVersion` must be NULL or a single string or number.")
}

#' @export
disableResume.reactiveVal <- function(x) {
  snapshot_mark_skipped(attr(x, ".impl", exact = TRUE))
  invisible(x)
}

#' @export
disableResume.reactivevalues <- function(x) {
  snapshot_mark_skipped(.subset2(x, "impl"))
  invisible(x)
}

#' @export
disableResume.Observer <- function(x) {
  x$.resumeSkip <- TRUE
  invisible(x)
}

#' @export
disableResume.ExtendedTask <- function(x) {
  snapshot_mark_skipped(attr(extended_task_state(x), ".impl", exact = TRUE))
  invisible(x)
}

# A node no registry tracks is never recorded, so there is nothing to mark.
snapshot_mark_skipped <- function(node, reason = "disableResume()") {
  state <- node$.snapshot
  if (!is.null(state)) {
    state$skip <- TRUE
    state$skipReason <- reason
  }
  invisible()
}

# Evaluates `expr`, whose reactiveVal() and reactiveValues() calls create
# sources Shiny keeps for itself: never recorded, counted or reported. They
# are marked as they are constructed, so they never trip the gate and never
# collide with another source's label. Force arguments that run user code
# before calling this.
snapshot_internal <- function(expr) {
  old <- .globals$snapshotInternal
  .globals$snapshotInternal <- TRUE
  on.exit(.globals$snapshotInternal <- old)
  expr
}
