#' @include utils.R
NULL

# Uploaded files outlive the process that received
# them by being copied beside the snapshot record, as bookmarking's
# serializerFileInput() copies them into a state directory, and come back
# through the `shiny.file` input handler bookmarks use.

# Which file inputs in `inputs` keep their files within `budget` bytes, in
# input order, with the record filename for each file.
snapshot_upload_plan <- function(inputs, fileInputs, budget) {
  keep <- list()
  omitted <- character(0)
  for (name in fileInputs) {
    value <- inputs[[name]]
    if (is.null(value)) next
    sizes <- file.info(value$datapath)$size
    if (anyNA(sizes) || sum(sizes) > budget) {
      omitted <- c(omitted, name)
      next
    }
    budget <- budget - sum(sizes)
    keep[[name]] <- snapshot_upload_names(name, value$datapath)
  }
  list(keep = keep, omitted = omitted)
}

# Copies the planned files into `dir` (a name already there is the same
# upload, so it is copied once), rewrites each file input's `datapath` to
# the bare record filenames, and removes files the record no longer uses.
snapshot_write_uploads <- function(snap, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, mode = "0700")
  for (name in names(snap$uploads)) {
    value <- snap$inputs[[name]]
    targets <- snap$uploads[[name]]
    dest <- file.path(dir, targets)
    missing <- !file.exists(dest)
    if (any(missing)) {
      if (!all(file.copy(value$datapath[missing], dest[missing]))) {
        stop("could not copy the files uploaded to input$", name)
      }
      Sys.chmod(dest[missing], "0600")
    }
    value$datapath <- targets
    snap$inputs[[name]] <- value
  }
  stale <- setdiff(list.files(dir), unlist(snap$uploads, use.names = FALSE))
  unlink(file.path(dir, stale))
  snap
}

# Restores every file input in `snapshot` through the `shiny.file` handler,
# which copies each file out of the session's restore context `dir` into a
# fresh temp directory and marks the input as a file input again. A
# value that cannot be restored becomes NULL, so the input starts empty and
# its dependents re-run.
snapshot_restore_uploads <- function(snapshot, session) {
  handler <- inputHandlers$get("shiny.file")
  for (name in snapshot$fileInputs) {
    value <- snapshot$inputs[[name]]
    if (is.null(value)) next
    restored <- tryCatch(
      withReactiveDomain(session, {
        dir <- session$restoreContext$dir
        if (is.null(dir) || !all(file.exists(file.path(dir, value$datapath)))) stop("files missing")
        handler(value, session, name)
      }),
      error = function(e) NULL
    )
    snapshot$inputs[name] <- list(restored)
  }
  snapshot
}

# `<id>-<row>-<digest><ext>`: the input id and row name the file, and
# a digest of the source path tells two uploads to one input apart.
snapshot_upload_names <- function(name, paths) {
  id <- gsub("[^A-Za-z0-9._-]", "_", name)
  ext <- tools::file_ext(paths)
  ext <- ifelse(nzchar(ext), paste0(".", ext), "")
  digest <- substr(vapply(paths, rlang::hash, character(1), USE.NAMES = FALSE), 1, 8)
  sprintf("%s-%d-%s%s", id, seq_along(paths), digest, ext)
}
