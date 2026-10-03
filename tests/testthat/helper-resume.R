# Points the process store at a fresh temp cache dir for the calling test.
local_snapshot_store_dir <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(R_USER_CACHE_DIR = dir, .local_envir = envir)
  snapshot_store_reset()
  withr::defer(snapshot_store_reset(), envir = envir)
  dir
}

# Calls enableResume(...) for the calling test, as an app.R would; sessions
# created afterwards resolve it.
local_enable_resume <- function(..., .env = parent.frame()) {
  old <- getShinyOption("resume")
  withr::defer(shinyOptions(resume = old), envir = .env)
  enableResume(...)
  invisible()
}

file_mode <- function(path) as.character(file.info(path)$mode)

# Runs `server` in a fresh MockShinySession exactly as testServer() does and
# returns the session; the caller drives it and reads `session$.snapshotRegistry`.
snapshot_session <- function(server, session = MockShinySession$new()) {
  server <- capture_env(server)
  withMockContext(session, {
    server(input = session$input, output = session$output, session = session)
  })
  session
}

# Rewrites `server` so its body's environment becomes `session$env`, as
# testServer() does.
capture_env <- function(server) {
  body(server) <- rlang::expr({
    session$setEnv(base::environment())
    !!body(server)
  })
  server
}

# Bounded: a callback scheduled in the future cannot make it spin.
drain_later <- function() {
  for (i in seq_len(20)) {
    if (later::loop_empty()) break
    later::run_now(0)
  }
}

spy_registry <- new.env(parent = emptyenv())

new_spy <- function(...) {
  spy <- new.env(parent = emptyenv())
  for (nm in c(...)) assign(nm, 0, envir = spy)
  spy_registry$spies <- c(spy_registry$spies, spy)
  spy
}

reset_spies <- function() {
  for (spy in spy_registry$spies) {
    for (nm in ls(spy, all.names = TRUE)) {
      if (is.numeric(get(nm, envir = spy))) assign(nm, 0, envir = spy) else rm(list = nm, envir = spy)
    }
  }
}

# The R value a source record carries (records hold serialized bytes).
node_value <- function(record) unserialize(record$bytes)

# Ids of the sources with an identity, without the leading "namespace|".
short_ids <- function(session) sub("^[^|]*\\|", "", names(session$.snapshotRegistry$snapshot()$sources))

# Parses R code without source references, which is what an installed
# package's functions look like (no srcrefs, so no assignment-derived labels).
snapshot_eval_no_srcref <- function(text, env = parent.frame()) {
  exprs <- parse(text = text, keep.source = FALSE)
  for (e in exprs) eval(e, env)
  invisible(env)
}

# Runs `server` in a mock session, drives it, takes its record, resumes a
# second mock session from it (through serialize(), as the store would), and
# returns everything a test needs. `live` is what the returning client
# reports (default: what the record has).
snapshot_roundtrip <- function(server, drive = function(session) NULL, live = NULL, fresh = FALSE,
                               second = MockShinySession$new()) {
  first <- snapshot_session(server)
  drive(first)
  first$flushReact()
  drain_later()
  record <- first$.snapshotRegistry$snapshot()
  record$identity <- snapshot_identity(first)
  record <- unserialize(serialize(record, NULL))
  reset_spies()
  server <- capture_env(server)
  outcome <- second$.snapshotRegistry$resume(
    record,
    live = list(inputs = live %||% record$inputs, clientData = list(), fileInputs = character(0)),
    fresh = fresh,
    run = function() withMockContext(second, server(input = second$input, output = second$output, session = second))
  )
  # Changed live inputs apply after the first flush, so their dependents run in the second.
  second$flushReact()
  second$flushReact()
  list(first = first, second = second, record = record, outcome = second$.snapshotRegistry$outcome(),
       report = second$resumeReport())
}
