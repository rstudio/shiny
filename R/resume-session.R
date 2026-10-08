#' @include utils.R
#' @include bookmark-state.R
NULL

# Splits a resume message's raw `inputs` (before input handlers) into the
# live-input set. `fileInputs` remembers which inputs are fileInput()s: the
# browser cannot re-send a file, so their NULL live value must not overwrite
# a restored upload.
snapshot_live_inputs <- function(rawInputs) {
  rawInputs <- if (is.list(rawInputs)) rawInputs else list()
  nms <- names(rawInputs) %||% character(0)
  isClientData <- grepl("^\\.clientdata_", nms)
  fileInputs <- sub(":shiny\\.file$", "", nms[grepl(":shiny\\.file$", nms)])
  inputs <- rawInputs[!isClientData]
  clientData <- rawInputs[isClientData]
  names(clientData) <- sub("^\\.clientdata_", "", names(clientData))
  list(inputs = inputs, clientData = clientData, fileInputs = fileInputs)
}

# Applies the registered input handlers to the live inputs (strips the
# ":type" suffixes), as the init path does for its whole payload.
snapshot_apply_handlers <- function(live, session) {
  live$inputs <- withReactiveDomain(session, applyInputHandlers(live$inputs, session))
  live
}

# The outputs the page reported (every bound output sends
# `.clientdata_output_<id>_hidden`) that the server has not defined.
snapshot_undefined_outputs <- function(clientData, defined) {
  nms <- grep("^output_.*_hidden$", names(clientData) %||% character(0), value = TRUE)
  setdiff(sub("^output_(.*)_hidden$", "\\1", nms), defined)
}

# A RestoreContext that answers restoreInput() but never wakes bookmarking:
# active = FALSE keeps onRestore()/onRestored() silent. `dir` is where the
# `shiny.file` handler finds a record's uploads.
snapshot_seed_restore_context <- function(session, inputs, dir = NULL) {
  ctx <- RestoreContext$new()
  ctx$set(active = FALSE, input = as.list(inputs), dir = dir)
  session$restoreContext <- ctx
  invisible(ctx)
}

# session$onResume() callbacks. Each is kept with the domain it was
# registered through (the session, or a module's scope) and runs under it.
ResumeCallbacks <- R6Class("ResumeCallbacks", cloneable = FALSE,
  public = list(
    register = function(fun, domain) {
      if (!is.function(fun) || length(formals(fun)) == 0L) {
        stop("`fun` must be a function that takes one argument")
      }
      i <- length(private$entries) + 1L
      private$entries[[i]] <- list(fun = fun, domain = domain)
      # The slot stays so the pass's positions do not shift.
      function() private$entries[i] <- list(NULL)
    },

    # Runs once per session, with `info = list(restored = restored)`, and
    # returns how many ran. Callbacks registered while it runs (a module
    # server started by a callback registers its own) run in the same pass;
    # those registered afterwards never do. An error propagates: the caller
    # ends the session.
    run = function(restored) {
      if (private$done) return(0L)
      private$done <- TRUE
      private$active <- TRUE
      on.exit(private$active <- FALSE, add = TRUE)
      info <- list(restored = restored)
      n <- 0L
      i <- 0L
      while (i < length(private$entries)) {
        i <- i + 1L
        entry <- private$entries[[i]]
        if (is.null(entry)) next
        n <- n + 1L
        withReactiveDomain(entry$domain, isolate(entry$fun(info)))
      }
      n
    },

    running = function() private$active
  ),
  private = list(entries = list(), done = FALSE, active = FALSE)
)

# The always-on stderr lines. `outcome` is the registry's outcome list, or a
# list with `resumed = "inputs"` and a `cause`.
snapshot_resume_log <- function(outcome, undefined = character(0)) {
  lines <- if (identical(outcome$resumed, "inputs")) {
    sprintf("Resumed from inputs only: %s", outcome$cause %||% "no saved state")
  } else {
    sprintf("Resumed: restored %d values and %d inputs; %d event handlers held back",
            outcome$values, outcome$inputs, length(outcome$skipped))
  }
  if (length(undefined)) {
    lines <- c(lines, sprintf("Outputs on the page with no definition after resume: %s", paste(undefined, collapse = ", ")))
  }
  lines
}

# The same counts as an OTel log event: the resume finishes after the
# session-start span has ended, so there is no session span to put them on.
snapshot_resume_otel <- function(session, outcome) {
  if (!has_otel_collect("session")) return(invisible())
  otel_log(
    "Resume",
    severity = "info",
    attributes = otel::as_attributes(c(
      otel_session_id_attrs(session),
      list(
        shiny.resume.resumed = outcome$resumed,
        shiny.resume.values = as.integer(outcome$values %||% 0L),
        shiny.resume.skipped = length(outcome$skipped),
        shiny.resume.callbacks = as.integer(outcome$callbacks %||% 0L)
      )
    ))
  )
  invisible()
}

# NULL when `record` has the structure resume() relies on, else what is
# wrong. A corrupt or foreign file must fall through before anything touches
# the session.
snapshot_shape_problem <- function(record) {
  if (!is.list(record)) return("not a list")
  if (!is.list(record$identity)) return("no identity block")
  if (!is.logical(record$complete) || length(record$complete) != 1) return("`complete` is not a flag")
  for (field in c("inputs", "clientData", "sources", "userData", "blocked")) {
    value <- record[[field]]
    if (!is.null(value) && !is.list(value)) return(sprintf("`%s` is not a list", field))
  }
  for (id in names(record$sources)) {
    if (!is.raw(record$sources[[id]]$bytes)) return(sprintf("source `%s` has no bytes", id))
  }
  fileInputs <- record$fileInputs
  if (!is.null(fileInputs) && !is.character(fileInputs)) return("`fileInputs` is not character")
  for (name in fileInputs) {
    value <- record$inputs[[name]]
    if (!is.null(value) && !is.list(value)) return(sprintf("file input `%s` is not a list", name))
  }
  ud <- names(record$userData)
  if (length(record$userData) && (is.null(ud) || anyNA(ud) || !all(nzchar(ud)))) return("`userData` has unnamed entries")
  NULL
}
