#' @include utils.R
NULL

# Uploaded files outlive the process that received
# them by being copied beside the snapshot record, as bookmarking's
# serializerFileInput() copies them into a state directory, and come back
# through the `shiny.file` input handler bookmarks use.

# Which of `fileInputs` (file input values by input id) keep their files
# within `budget` bytes, in input order, with the record filename for each file.
snapshot_upload_plan <- function(fileInputs, budget) {
  keep <- list()
  omitted <- character(0)
  for (name in names(fileInputs)) {
    sizes <- file.info(fileInputs[[name]]$datapath)$size
    if (anyNA(sizes) || sum(sizes) > budget) {
      omitted <- c(omitted, name)
      next
    }
    budget <- budget - sum(sizes)
    keep[[name]] <- snapshot_upload_names(name, fileInputs[[name]]$datapath)
  }
  list(keep = keep, omitted = omitted)
}

# Copies the planned files into `dir` (a name already there is the same
# upload, so it is copied once), rewrites each file input's `datapath` to
# the bare record filenames. The previous record's files stay until the new
# record is written (snapshot_prune_uploads()).
snapshot_write_uploads <- function(snap, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, mode = "0700")
  for (name in names(snap$uploads)) {
    value <- snap$fileInputs[[name]]
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
    snap$fileInputs[[name]] <- value
  }
  snap
}

# Removes the files in `dir` the record just written does not reference.
snapshot_prune_uploads <- function(dir, keep) {
  stale <- setdiff(list.files(dir), keep)
  unlink(file.path(dir, stale))
  invisible()
}

# Restores every file input in `record` through the `shiny.file` handler,
# which copies each file out of the session's restore context `dir` into a
# fresh temp directory and marks the input as a file input again. A value
# that cannot be restored becomes NULL, so the input starts empty and its
# dependents re-run.
snapshot_restore_uploads <- function(record, session) {
  handler <- inputHandlers$get("shiny.file")
  for (name in names(record$fileInputs)) {
    value <- record$fileInputs[[name]]
    restored <- tryCatch(
      withReactiveDomain(session, {
        dir <- session$restoreContext$dir
        if (is.null(dir) || !all(file.exists(file.path(dir, value$datapath)))) stop("files missing")
        handler(value, session, name)
      }),
      error = function(e) NULL
    )
    record$fileInputs[name] <- list(restored)
  }
  record
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
