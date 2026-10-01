#' @include utils.R
#' @include resume-options.R
NULL

# Disk store for resume snapshots over cachem's
# cache_disk(): one file per key, TTL by max_age, atomic writes (cachem
# writes to a temp file and renames). cachem sets no permissions, so writes
# run under a restrictive umask and files are chmod'ed owner-only afterwards.
# "fifo" eviction keeps reads from touching mtime, which is also the TTL clock.
SnapshotStore <- R6Class("SnapshotStore", cloneable = FALSE,
  public = list(
    initialize = function(dir, max_age = Inf, evict = "fifo") {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
      }
      if (!dir.exists(dir)) {
        stop("cannot create directory ", dir)
      }
      Sys.chmod(dir, "0700")
      private$dir <- dir
      private$cache <- cachem::cache_disk(
        dir = dir, max_age = max_age, evict = evict, warn_ref_objects = FALSE
      )
      private$deleteCallbacks <- Map$new()
      self$sweep()
    },
    write = function(key, value) {
      old <- Sys.umask("077")
      on.exit(Sys.umask(old), add = TRUE)
      private$cache$set(key, value)
      Sys.chmod(private$path(key), "0600")
      invisible(TRUE)
    },
    read = function(key) {
      value <- private$cache$get(key)
      if (cachem::is.key_missing(value)) NULL else value
    },
    exists = function(key) {
      private$cache$exists(key)
    },
    delete = function(key) {
      private$cache$remove(key)
      callback <- private$deleteCallbacks$get(key)
      if (!is.null(callback)) {
        private$deleteCallbacks$remove(key)
        callback()
      }
      invisible(TRUE)
    },
    # Removes `key` for adoption by a session in this process. Unlike
    # delete(), the onDelete() callback is called with `transfer = TRUE`, so
    # it hands over what it retained instead of releasing it, and its result
    # (for upload retention, the directories) is returned. NULL without one.
    take = function(key) {
      private$cache$remove(key)
      callback <- private$deleteCallbacks$get(key)
      if (is.null(callback)) return(NULL)
      private$deleteCallbacks$remove(key)
      callback(transfer = TRUE)
    },
    # Prunes expired records and runs the onDelete() callback of every key
    # that no longer exists; cachem's own pruning never calls them.
    sweep = function() {
      private$cache$prune()
      for (key in private$deleteCallbacks$keys()) {
        if (!private$cache$exists(key)) self$delete(key)
      }
      invisible(TRUE)
    },
    # Run `callback` when this process deletes `key` (used to defer upload
    # directory removal until the snapshot that references it is gone).
    onDelete = function(key, callback) {
      private$deleteCallbacks$set(key, callback)
      invisible()
    },
    directory = function() private$dir
  ),
  private = list(
    dir = NULL,
    cache = NULL,
    deleteCallbacks = NULL,
    path = function(key) file.path(private$dir, paste0(key, ".rds"))
  )
)

# A platform's registered bookmark save.interface provides a
# per-id directory; otherwise the user cache dir. Never the app directory.
snapshot_dir <- function() {
  saveInterface <- getShinyOption("save.interface", default = NULL)
  if (is.function(saveInterface)) {
    dir <- NULL
    saveInterface("snapshots", function(stateDir) dir <<- stateDir)
    if (is.character(dir) && length(dir) == 1 && !is.na(dir) && nzchar(dir)) return(dir)
  }
  fallback <- file.path(tools::R_user_dir("shiny", "cache"), "resume")
  # Other apps run by the same user share this directory, and with it TTL
  # pruning, so say so rather than fall back silently.
  if (is.function(saveInterface)) {
    message("Resume: the registered save.interface gave no usable directory; using ", fallback)
  }
  fallback
}

# One store per directory per process. A directory that cannot be created
# disables the feature for that directory with a single message.
# Never throws: the writer calls this from writeNow(). Resolving the
# directory runs a platform's save.interface, so it is guarded too and
# resolved once per process; a failure there is recorded
# under "<unresolved>" so it is neither retried nor reported again.
snapshot_store <- function() {
  if (is.null(.globals$snapshotStores)) .globals$snapshotStores <- Map$new()
  dir <- "<unresolved>"
  if (is.null(.globals$snapshotDir) && .globals$snapshotStores$containsKey(dir)) return(NULL)
  store <- tryCatch({
    resolved <- .globals$snapshotDir %||% snapshot_dir()
    if (!is.character(resolved) || length(resolved) != 1 || is.na(resolved) || !nzchar(resolved)) {
      stop("no usable snapshot directory")
    }
    .globals$snapshotDir <- dir <- resolved
    .globals$snapshotStores$get(dir) %||%
      SnapshotStore$new(dir, max_age = snapshot_ttl)
  }, error = function(e) {
    if (!.globals$snapshotStores$containsKey(dir)) {
      message("Resume disabled for this process: ", conditionMessage(e))
    }
    FALSE
  })
  .globals$snapshotStores$set(dir, store)
  if (identical(store, FALSE)) NULL else store
}

# Removes `dirs` when `key` is deleted from `store` or after `ttl` seconds,
# whichever comes first; `store$take(key)` instead returns them, still on
# disk, for the adopting session to own. Top-level functions, so the closures capture only
# these arguments: built inside the session they would pin it, its inputs
# and userData for the whole TTL. The TTL backstop uses
# Shiny's timer, not later::later(): a day-long callback in the later loop
# would make every loop_empty() drain (wait_for_it(), promise code) spin.
snapshot_retain_upload_dirs <- function(store, key, dirs, ttl) {
  rm_once <- snapshot_deferred_unlink(dirs)
  cancelTtl <- scheduleTask(1000 * ttl, rm_once)
  store$onDelete(key, function(transfer = FALSE) {
    cancelTtl()
    if (transfer) return(dirs)
    rm_once()
  })
  invisible()
}

snapshot_deferred_unlink <- function(dirs) {
  force(dirs)
  done <- FALSE
  function() {
    if (!done) {
      done <<- TRUE
      unlink(dirs, recursive = TRUE)
    }
  }
}

snapshot_store_reset <- function() {
  .globals$snapshotStores <- NULL
  .globals$snapshotDir <- NULL
  invisible()
}

# The token gates a session's whole state, so take it from the operating
# system's random source where there is one and fall back to Shiny's id
# generator elsewhere.
snapshot_token_new <- function() {
  if (file.exists("/dev/urandom")) {
    con <- file("/dev/urandom", "rb", raw = TRUE)
    on.exit(close(con))
    bytes <- readBin(con, "raw", 16L)
    if (length(bytes) == 16L) {
      return(paste(format(as.hexmode(as.integer(bytes)), width = 2), collapse = ""))
    }
  }
  createUniqueId(16)
}

snapshot_token_valid <- function(token) {
  is.character(token) && length(token) == 1 && !is.na(token) && grepl("^[0-9a-f]{32}$", token)
}

snapshot_token_key <- function(token) rlang::hash(token)

snapshot_format_version <- 1L

# App identity block: no hash of app sources, so a code
# change keeps the state of every node whose body did not change; body and
# path hashes carry code changes per node. (Autoreload itself reloads the
# page, which this PR treats as a fresh start; the reload PR extends this.)
snapshot_identity <- function(session) {
  list(
    appPath = normalizePath(getShinyOption("appDir", default = getwd()), mustWork = FALSE),
    formatVersion = snapshot_format_version,
    appVersion = session$.resumeSettings$appVersion,
    user = session$user,
    groups = session$groups,
    uiHash = snapshot_ui_hash()
  )
}

# Fingerprint of the page the client is showing, recorded by shinyApp() on
# the app object: the rendered HTML for a static UI,
# the deparsed body for a function UI, and for ui.R apps a function that
# hashes the file when called. NULL when unknown, which disables the check.
snapshot_ui_hash <- function() {
  app <- getCurrentAppState()$app
  h <- app$uiHash
  if (is.function(h)) h <- tryCatch(h(), error = function(e) NULL)
  if (is.character(h) && length(h) == 1 && !is.na(h)) h else NULL
}

snapshot_ui_fingerprint <- function(ui) {
  if (is.function(ui)) return(snapshot_hash_expr(fn_body(ui)))
  rlang::hash(as.character(htmltools::renderTags(ui)$html))
}

snapshot_identity_mismatch <- function(snapshot, session, enforceUser = TRUE) {
  saved <- snapshot$identity
  if (!is.list(saved)) return("unknown snapshot format")
  now <- snapshot_identity(session)
  if (!identical(saved$formatVersion, now$formatVersion)) return("snapshot format version differs")
  if (!identical(saved$appPath, now$appPath)) return("app path differs")
  if (!identical(saved$appVersion, now$appVersion)) return("appVersion differs")
  if (enforceUser && (!identical(saved$user, now$user) || !identical(saved$groups, now$groups))) return("user differs")
  NULL
}
