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

# Run `server` in a mock session, drive it, snapshot it,
# adopt into a second mock session, return everything a test needs.
snapshot_roundtrip <- function(server, drive = function(session) NULL, live = NULL,
                               dom = "intact", policy = "reconnect", second = MockShinySession$new()) {
  first <- snapshot_session(server)
  drive(first)
  first$flushReact()
  drain_later()
  pol <- switch(policy, reconnect = snapshot_policy_reconnect(), stop("unknown policy ", policy))
  snap <- first$.snapshotRegistry$snapshot(pol)
  snap$identity <- snapshot_identity(first)
  snap <- unserialize(serialize(snap, NULL))  # cross the process boundary
  adopt_snapshot_into(snap, server, second, live = live %||% snap$inputs, dom = dom)
  list(first = first, second = second, snapshot = snap,
       report = second$resumeReport(), outcome = second$.snapshotRegistry$adoptOutcome())
}

# A mock output's promise chain settles over several event-loop ticks; the
# real writer is debounced, so it snapshots after they have run. Bounded, so
# a callback scheduled in the future cannot make it spin.
drain_later <- function() {
  for (i in seq_len(20)) {
    if (later::loop_empty()) break
    later::run_now(0)
  }
}

# Adopts `snap` into `session` running `server`. Spies are shared by both
# sessions of a round trip, so they are reset first: assertions after this
# measure the adopting session only.
adopt_snapshot_into <- function(snap, server, session = MockShinySession$new(), live = snap$inputs,
                                flush = TRUE, dom = "intact",
                                liveSet = list(inputs = live, clientData = list(), fileInputs = character(0))) {
  reset_spies()
  server <- capture_env(server)
  run <- function() {
    withMockContext(session, server(input = session$input, output = session$output, session = session))
  }
  info <- list(from = "reconnect", resumed = "snapshot", dom = dom)
  session$.snapshotRegistry$adopt(snap, liveSet, run, info)
  if (flush) {
    # Changed live inputs apply after the first flush, so their dependents run in the second.
    session$flushReact()
    session$flushReact()
  }
  session
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

# Ids with the leading "namespace|creator|" stripped, for readable assertions.
snapshot_short_ids <- function(session) {
  ids <- session$.snapshotRegistry$ids()
  sub("^[^|]*\\|[^|]*\\|", "", ids)
}

# The R value a source record carries (records hold serialized bytes).
node_value <- function(record) unserialize(record$bytes)

# Parses R code without source references, which is what an installed
# package's functions look like (no srcrefs, so no assignment-derived labels).
snapshot_eval_no_srcref <- function(text, env = parent.frame()) {
  exprs <- parse(text = text, keep.source = FALSE)
  for (e in exprs) eval(e, env)
  invisible(env)
}

# Points the process store at a fresh temp cache dir for the calling test.
local_snapshot_store_dir <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(R_USER_CACHE_DIR = dir, .local_envir = envir)
  snapshot_store_reset()
  withr::defer(snapshot_store_reset(), envir = envir)
  dir
}

file_mode <- function(path) as.character(file.info(path)$mode)

e2e_start_app <- function(port, store_dir, log_file, env = character()) {
  pkg_dir <- normalizePath(testthat::test_path("..", ".."))
  app_dir <- normalizePath(testthat::test_path("apps", "resume-e2e"))
  callr::r_bg(
    function(pkg_dir, app_dir, port, log_file, store_dir, env) {
      Sys.setenv(RESUME_E2E_LOG = log_file, R_USER_CACHE_DIR = store_dir)
      if (length(env)) do.call(Sys.setenv, as.list(env))
      if (file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
        pkgload::load_all(pkg_dir, quiet = TRUE)
      } else {
        library(shiny)
      }
      shiny::runApp(app_dir, port = port, launch.browser = FALSE)
    },
    args = list(pkg_dir = pkg_dir, app_dir = app_dir, port = port, log_file = log_file,
                store_dir = store_dir, env = env),
    stdout = "|", stderr = "|"
  )
}

e2e_wait_until <- function(pred, timeout = 20, what = "condition", on_timeout = function() NULL) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (isTRUE(pred())) return(invisible(TRUE))
    Sys.sleep(0.25)
  }
  stop("Timed out waiting for ", what, paste0("\n", on_timeout(), collapse = ""))
}

e2e_port_open <- function(port) {
  con <- try(suppressWarnings(socketConnection("127.0.0.1", port, timeout = 1)), silent = TRUE)
  if (inherits(con, "try-error")) return(FALSE)
  close(con)
  TRUE
}

e2e_js <- function(b, code) b$Runtime$evaluate(code)$result$value

e2e_text <- function(b, selector) {
  e2e_js(b, sprintf("(document.querySelector(%s) || {}).textContent || ''", jsonlite::toJSON(selector, auto_unbox = TRUE)))
}

e2e_click <- function(b, selector) {
  e2e_js(b, sprintf("document.querySelector(%s).click()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
  invisible()
}

e2e_wait_for_snapshot <- function(store_dir, since, timeout = 20) {
  e2e_wait_until(function() {
    files <- list.files(store_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
    length(files) > 0 && any(file.mtime(files) >= since)
  }, timeout = timeout, what = "a snapshot written after the last change")
}

skip_if_no_e2e <- function() {
  skip_if_not_shinytest2()
  skip_if_not_installed("callr")
  skip_if_not_installed("chromote")
  skip_if_not_installed("pkgload")
  skip_if(
    is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
    "Chrome not available"
  )
}

# Calls enableResume(...) for the calling test, as an app.R would; sessions
# created afterwards resolve it. shinyApp() captures and clears it, so tests
# that only need resume on or off set SHINY_RESUME instead.
local_enable_resume <- function(..., .env = parent.frame()) {
  old <- getShinyOption("resume")
  withr::defer(shinyOptions(resume = old), envir = .env)
  enableResume(...)
  invisible()
}
