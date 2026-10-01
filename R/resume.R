#' @include utils.R
NULL

#' Resume sessions after an interruption
#'
#' @description
#' Shiny saves each session's state as it goes. When the connection drops
#' and the page stays open, the reconnected session **resumes** from that
#' saved state instead of starting over: [reactiveVal()] and
#' [reactiveValues()] keep their values, `input` and `session$userData` come
#' back, observers do not run again, and [reactive()] expressions recompute
#' when they are next read. Inputs that changed while disconnected are applied
#' on top.
#'
#' `enableResume()` turns resume on for an app and configures it;
#' `disableResume()` turns it off. Call either at the top level of `app.R`
#' (before [shinyApp()]) or in `global.R`, not in `server.R` or inside the
#' server function.
#' Like [enableBookmarking()], the call applies to the app created next, so
#' two apps in one R process keep their own settings. When an app calls both,
#' the last call wins.
#'
#' `disableResume(x)` instead leaves one reactive out:
#'
#' * On a [reactiveVal()] or [reactiveValues()] object, its value is not
#'   saved; after a resume it holds its initial value and everything that
#'   read it runs again.
#' * On an observer ([observe()], [observeEvent()]), it is not restored; it
#'   runs after a resume exactly as it does when the app starts, so an
#'   observer whose job is to establish side state (open a connection, create
#'   a client object) can rebuild it. `ignoreInit = TRUE` still applies.
#' * On a render function, the output is not restored and renders again at
#'   the first flush, for outputs that depend on the current time or on
#'   random draws.
#'
#' Pipe render functions into it (`output$clock <- renderText(...) |>
#' disableResume()`). For `reactiveVal()`, `reactiveValues()` and
#' `observe()`, call `disableResume(x)` as its own statement or give the
#' reactive a `label`: piping the constructor loses the label Shiny infers
#' from the assignment, and with it a stable identity. Every reactive left
#' out has a row in `session$resumeReport()` with reason `"disableResume()"`.
#'
#' @section Who decides:
#' Resume is resolved once per app, and the first of these that applies wins:
#'
#' 1. The app: its last `enableResume()` or `disableResume()` call.
#' 2. The hosting platform: the `SHINY_RESUME` environment variable (`TRUE`
#'    or `FALSE`). A platform that turns resume on gets `enableResume()`'s
#'    defaults.
#' 3. The default: on when running locally, off under Shiny Server or Posit
#'    Connect.
#'
#' With resume on, the browser retries a lost connection on any server, up
#' to ten times, and `session$allowReconnect()` has nothing to add (see
#' [session]).
#'
#' @section Saved state:
#' Saved state is written to the hosting platform's bookmark storage when it
#' provides one, otherwise to `tools::R_user_dir("shiny", "cache")`; on POSIX
#' systems only the owner can read it. It is kept for 24 hours. A value that
#' cannot be serialized (it holds a connection, an R6 object, or a closure) is
#' not saved, and `session$resumeReport()` says so.
#'
#' @param appVersion `NULL`, or a version (a string or number) recorded with
#'   the saved state. Change it in a deploy that changes what the app's state
#'   means, so sessions saved by the previous version start fresh.
#' @param maxSize Bytes; the total size of a session's saved state. Over it,
#'   the largest values are left out first.
#' @param maxNodeSize Bytes; a reactive value whose serialized size exceeds
#'   this is not saved.
#' @param debounce Milliseconds of quiet after a change before the state is
#'   saved. While changes keep arriving it is saved at least every ten such
#'   intervals.
#' @param x Optional. A reactive value, `reactiveValues()` object, observer,
#'   or render function to leave out of resume. Without it, `disableResume()`
#'   turns resume off for the app.
#' @return `enableResume()` and `disableResume()` return `NULL` invisibly;
#'   `disableResume(x)` returns `x` invisibly.
#' @seealso [session] for `session$onResumed()` and `session$resumeReport()`.
#' @examples
#' \dontrun{
#' # app.R
#' library(shiny)
#' enableResume(appVersion = "2")
#'
#' ui <- fluidPage(textOutput("clock"))
#' server <- function(input, output, session) {
#'   conn <- NULL
#'   connector <- observe({
#'     conn <<- file(tempfile(), open = "w")
#'   })
#'   disableResume(connector)
#'
#'   output$clock <- renderText(format(Sys.time())) |> disableResume()
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
    "disableResume() expects a reactiveVal, reactiveValues, observer, or render function, not an object of class ",
    paste(class(x), collapse = "/"), "."
  ))
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
  snapshot_mark_skipped(x)
  invisible(x)
}

#' @export
disableResume.shiny.render.function <- function(x) {
  attr(x, "disableResume") <- TRUE
  invisible(x)
}

# A node no registry tracks is never snapshotted, so there is nothing to mark.
snapshot_mark_skipped <- function(node) {
  if (!is.null(node$.snapshot)) node$.snapshot$skip <- TRUE
  invisible()
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
