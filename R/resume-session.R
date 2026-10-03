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

# The record's values of the bound inputs whose value on the fresh page
# differs. File inputs (no binding can set one) and clientData are never
# pushed. `live` is handler-applied.
snapshot_push_inputs <- function(record, live) {
  ids <- setdiff(intersect(names(live$inputs), names(record$inputs)), c(record$fileInputs, live$fileInputs))
  push <- list()
  for (id in ids) {
    if (!identical(live$inputs[[id]], record$inputs[[id]])) push[id] <- list(record$inputs[[id]])
  }
  push
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

# The bookmark RestoreContext server.R's init builds from the page's
# `.clientdata_url_search`, or NULL when bookmarking is disabled or the
# context cannot be built.
snapshot_bookmark_restore_context <- function(urlSearch) {
  if (identical(getShinyOption("bookmarkStore", default = "disable"), "disable")) return(NULL)
  tryCatch(RestoreContext$new(urlSearch), error = function(e) NULL)
}

# The always-on stderr lines. `outcome` is the registry's outcome
# list, or a list with `resumed = "inputs"` and a `cause`.
snapshot_resume_log <- function(from, outcome, cause = NULL) {
  if (identical(outcome$resumed, "inputs")) {
    return(sprintf("Resumed (%s) from inputs only: %s", from, cause %||% outcome$cause %||% "no saved state"))
  }
  reran <- outcome$reran %||% list()
  lines <- sprintf("Resumed (%s): restored %d values and %d inputs; %d event handlers skipped, %d re-ran",
                   from, outcome$values, outcome$inputs, length(outcome$skipped), length(reran))
  if (length(reran)) {
    shown <- utils::head(reran, 5)
    lines <- c(lines, vapply(shown, function(r) sprintf("  %s: %s", r$label, r$why), character(1)))
    if (length(reran) > 5) lines <- c(lines, sprintf("  and %d more", length(reran) - 5))
  }
  lines
}

# The same counts as an OTel log event: the resume finishes after the
# session-start span has ended, so there is no session span to put them on.
snapshot_resume_otel <- function(session, from, outcome) {
  if (!has_otel_collect("session")) return(invisible())
  otel_log(
    "Resume",
    severity = "info",
    attributes = otel::as_attributes(c(
      otel_session_id_attrs(session),
      list(
        shiny.resume.from = from,
        shiny.resume.resumed = outcome$resumed,
        shiny.resume.values = as.integer(outcome$values %||% 0L),
        shiny.resume.skipped = length(outcome$skipped),
        shiny.resume.reran = length(outcome$reran)
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
