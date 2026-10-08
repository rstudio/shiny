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
    initialize = function(dir, max_age = snapshot_ttl, max_size = snapshot_store_max_size,
                          max_n = snapshot_store_max_n, evict = "fifo") {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
      }
      if (!dir.exists(dir)) {
        stop("cannot create directory ", dir)
      }
      Sys.chmod(dir, "0700")
      private$dir <- dir
      private$maxAge <- max_age
      private$consumed <- fastmap::fastmap()
      private$cache <- cachem::cache_disk(
        dir = dir, max_size = max_size, max_n = max_n, max_age = max_age,
        evict = evict, warn_ref_objects = FALSE
      )
      self$sweep()
    },
    # FALSE, without writing, for a key a resume already claimed: an old
    # session whose socket close arrives late must not bring it back.
    write = function(key, value) {
      if (private$consumed$has(key)) return(invisible(FALSE))
      old <- Sys.umask("077")
      on.exit(Sys.umask(old), add = TRUE)
      private$cache$set(key, value)
      Sys.chmod(private$path(key), "0600")
      invisible(TRUE)
    },
    # Single use: deletes the record and its files and refuses later writes
    # to the key in this process.
    claim = function(key) {
      private$consumed$set(key, as.numeric(Sys.time()))
      self$delete(key)
    },
    claimed = function(key) private$consumed$has(key),
    read = function(key) {
      value <- private$cache$get(key)
      if (cachem::is.key_missing(value)) NULL else value
    },
    delete = function(key) {
      private$cache$remove(key)
      unlink(self$filesDir(key), recursive = TRUE)
      invisible(TRUE)
    },
    # Prunes expired records, then removes `<key>-files/` directories whose
    # record is gone; cachem's own pruning never sees them. A directory
    # younger than a minute is spared: another process may be between
    # copying uploads and writing the record they belong to.
    sweep = function() {
      private$cache$prune()
      for (d in list.files(private$dir, pattern = "-files$")) {
        key <- sub("-files$", "", d)
        hasRecord <- tryCatch(private$cache$exists(key), error = function(e) TRUE)
        if (hasRecord) next
        path <- file.path(private$dir, d)
        if (isTRUE(as.numeric(Sys.time()) - as.numeric(file.mtime(path)) > 60)) unlink(path, recursive = TRUE)
      }
      now <- as.numeric(Sys.time())
      for (key in private$consumed$keys()) {
        if (now - private$consumed$get(key) > private$maxAge) private$consumed$remove(key)
      }
      invisible(TRUE)
    },
    # Where the uploads a record references are copied.
    filesDir = function(key) file.path(private$dir, paste0(key, "-files")),
    directory = function() private$dir
  ),
  private = list(
    dir = NULL,
    cache = NULL,
    maxAge = NULL, consumed = NULL,
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
      SnapshotStore$new(dir)
  }, error = function(e) {
    if (!.globals$snapshotStores$containsKey(dir)) {
      message("Resume disabled for this process: ", conditionMessage(e))
    }
    FALSE
  })
  .globals$snapshotStores$set(dir, store)
  if (identical(store, FALSE)) NULL else store
}

snapshot_store_reset <- function() {
  .globals$snapshotStores <- NULL
  .globals$snapshotDir <- NULL
  invisible()
}

# 128 bits from the operating system's random source. On Windows, from
# openssl when it is installed, else Shiny's id generator (R's RNG; said
# so in ?enableResume).
snapshot_token_new <- function() {
  if (file.exists("/dev/urandom")) {
    con <- file("/dev/urandom", "rb", raw = TRUE)
    on.exit(close(con))
    bytes <- readBin(con, "raw", 16L)
    if (length(bytes) == 16L) return(paste(format(as.hexmode(as.integer(bytes)), width = 2), collapse = ""))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(paste(format(as.hexmode(as.integer(openssl::rand_bytes(16))), width = 2), collapse = ""))
  }
  createUniqueId(16)
}

snapshot_token_valid <- function(token) {
  is.character(token) && length(token) == 1 && !is.na(token) && grepl("^[0-9a-f]{32}$", token)
}

# The record key folds in the app path, so a record another app wrote into
# the shared cache directory is never read, let alone unserialize()d.
snapshot_record_key <- function(token, appPath = snapshot_app_path()) {
  rlang::hash(c(token, appPath))
}

snapshot_app_path <- function() {
  normalizePath(getShinyOption("appDir", default = getwd()), mustWork = FALSE)
}

snapshot_format_version <- 3L

# App identity block: what must match for a record to be read.
snapshot_identity <- function(session) {
  appVersion <- session$.resumeSettings$appVersion
  list(
    appPath = snapshot_app_path(),
    formatVersion = snapshot_format_version,
    appVersion = appVersion,
    # Without a declared version the app's code stands in for one, so a
    # deploy that changes it falls back before the server function runs.
    codeHash = if (is.null(appVersion)) snapshot_session_code_hash(session),
    user = session$user,
    groups = session$groups
  )
}

snapshot_identity_mismatch <- function(snapshot, session, enforceUser = TRUE) {
  saved <- snapshot$identity
  if (!is.list(saved)) return("unknown snapshot format")
  now <- snapshot_identity(session)
  if (!identical(saved$formatVersion, now$formatVersion)) return("snapshot format version differs")
  if (!identical(saved$appPath, now$appPath)) return("app path differs")
  if (!identical(saved$appVersion, now$appVersion)) return("appVersion differs")
  if (!identical(saved$codeHash, now$codeHash)) return("app code changed")
  if (enforceUser && (!identical(saved$user, now$user) || !identical(saved$groups, now$groups))) return("user differs")
  NULL
}

# A hash of the names and contents of every .R/.r file directly in the app
# directory and in its R/ subdirectory, in C-locale order; NULL when there
# are none. Packages, data and www/ are not covered: apps whose saved state
# depends on them declare appVersion.
snapshot_code_hash <- function(dir = snapshot_app_path()) {
  rel <- c(
    list.files(dir, pattern = "[.][rR]$"),
    file.path("R", list.files(file.path(dir, "R"), pattern = "[.][rR]$"))
  )
  rel <- rel[!dir.exists(file.path(dir, rel))]
  if (!length(rel)) return(NULL)
  rel <- rel[order(rel, method = "radix")]
  rlang::hash(list(rel, vapply(file.path(dir, rel), rlang::hash_file, character(1), USE.NAMES = FALSE)))
}

snapshot_session_code_hash <- function(session) {
  reg <- session$.snapshotRegistry
  if (is.null(reg)) snapshot_code_hash() else reg$codeHash()
}
