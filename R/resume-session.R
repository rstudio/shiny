#' @include utils.R
#' @include bookmark-state.R
NULL

# Splits a resume message's raw `inputs` (before input handlers) into the
# live-input set the adopt algorithm consumes. `fileInputs` remembers which
# inputs are fileInput()s: the browser cannot re-send a file, so their NULL
# live value must not overwrite an adopted upload.
snapshot_live_inputs <- function(rawInputs) {
  rawInputs <- rawInputs %||% list()
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

# A RestoreContext that answers restoreInput() but never wakes bookmarking:
# active = FALSE keeps onRestore()/onRestored() silent.
snapshot_seed_restore_context <- function(session, inputs) {
  ctx <- RestoreContext$new()
  ctx$set(active = FALSE, input = as.list(inputs))
  session$restoreContext <- ctx
  invisible(ctx)
}

# Amendment 3.8: the always-on stderr block, one shape for every cause.
snapshot_resume_log <- function(outcome, info, cause = NULL) {
  if (identical(info$resumed, "inputs")) {
    return(sprintf("Resumed (%s) from inputs only: %s", info$from, cause %||% "no snapshot"))
  }
  counts <- snapshot_resume_counts(outcome)
  nAdopted <- counts$adopted
  fb <- counts$reranRows
  nFallback <- counts$reran
  lines <- sprintf("Resumed (%s): restored %d of %d reactive nodes, %d re-ran", info$from, nAdopted, nAdopted + nFallback, nFallback)
  shown <- utils::head(fb, 5)
  if (NROW(shown)) {
    lines <- c(lines, sprintf("  %s: %s", sub("^[^|]*\\|[^|]*\\|", "", shown$id), shown$reason))
  }
  if (nFallback > 5) {
    lines <- c(lines, sprintf("  and %d more; see session$resumeReport()", nFallback - 5))
  }
  if (length(outcome$hazards)) {
    lines <- c(lines, sprintf("%d adopted observers assign to closure variables; see the hazard column of session$resumeReport()", length(outcome$hazards)))
  }
  lines
}

# The resume report's counts, as a log event: the resume finishes after the
# session-start span has ended, so there is no session span to put them on.
snapshot_resume_otel <- function(session, info, outcome) {
  if (!has_otel_collect("session")) return(invisible())
  counts <- snapshot_resume_counts(outcome)
  otel_log(
    "Resume",
    severity = "info",
    attributes = otel::as_attributes(c(
      otel_session_id_attrs(session),
      list(
        shiny.resume.from = info$from,
        shiny.resume.resumed = info$resumed,
        shiny.resume.adopted = counts$adopted,
        shiny.resume.reran = counts$reran
      )
    ))
  )
  invisible()
}

# The counts the log and the OTel attributes share. They cover observers and
# outputs: `adopted` holds only those, and a source or input fallback is not
# something that "re-ran". `outcome` is NULL on an inputs-only resume.
snapshot_resume_counts <- function(outcome) {
  fb <- outcome$fallbacks
  rows <- if (is.null(fb)) NULL else fb[fb$kind %in% c("observer", "output"), , drop = FALSE]
  list(adopted = length(outcome$adopted), reran = NROW(rows), reranRows = rows)
}

# NULL when `snapshot` has the structure adopt() relies on, else what is
# wrong. A corrupt or foreign file must fall back to inputs-only before
# adoption touches the session.
snapshot_shape_problem <- function(snapshot) {
  if (!is.list(snapshot)) return("not a list")
  if (!is.list(snapshot$identity)) return("no identity block")
  for (field in c("inputs", "clientData", "nodes", "userData")) {
    value <- snapshot[[field]]
    if (!is.null(value) && !is.list(value)) return(sprintf("`%s` is not a list", field))
  }
  fileInputs <- snapshot$fileInputs
  if (!is.null(fileInputs) && !is.character(fileInputs)) return("`fileInputs` is not character")
  for (name in fileInputs) {
    value <- snapshot$inputs[[name]]
    if (!is.null(value) && !is.list(value)) return(sprintf("file input `%s` is not a list", name))
  }
  ud <- names(snapshot$userData)
  if (length(snapshot$userData) && (is.null(ud) || anyNA(ud) || !all(nzchar(ud)))) return("`userData` has unnamed entries")
  NULL
}
