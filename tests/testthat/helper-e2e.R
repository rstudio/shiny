# Browser-level tests: the app runs in a child R process, Chrome is driven
# through chromote. Apps live in tests/testthat/apps/<app>/app.R and may read
# E2E_* environment variables set by the test.

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

e2e_start_app <- function(port, log_file, env = character(), app = "reconnect-e2e", store_dir = NULL) {
  pkg_dir <- normalizePath(testthat::test_path("..", ".."))
  app_dir <- normalizePath(testthat::test_path("apps", app))
  callr::r_bg(
    function(pkg_dir, app_dir, port, log_file, env, store_dir) {
      Sys.setenv(E2E_LOG = log_file)
      # Resume records go to a directory the test can look into.
      if (!is.null(store_dir)) Sys.setenv(R_USER_CACHE_DIR = store_dir)
      if (length(env)) do.call(Sys.setenv, as.list(env))
      pkgload::load_all(pkg_dir, quiet = TRUE)
      shiny::runApp(app_dir, port = port, launch.browser = FALSE)
    },
    args = list(pkg_dir = pkg_dir, app_dir = app_dir, port = port, log_file = log_file, env = env,
                store_dir = store_dir),
    stdout = "|", stderr = "|"
  )
}

# Starts `app`, opens it in Chrome, waits until `ready` shows `ready_text`.
# Cleans up in the caller's frame.
e2e_session <- function(env = character(), app = "reconnect-e2e", ready = "#count",
                        ready_text = "Count: 0", path = "/") {
  envir <- parent.frame()
  port <- httpuv::randomPort()
  work_dir <- withr::local_tempdir(.local_envir = envir)
  log_file <- file.path(work_dir, "app.log")
  store_dir <- file.path(work_dir, "store")
  p <- e2e_start_app(port, log_file, env, app = app, store_dir = store_dir)
  withr::defer(try(p$kill(), silent = TRUE), envir = envir)
  e2e_wait_until(
    function() e2e_port_open(port),
    what = "app to start",
    on_timeout = function() p$read_all_error_lines()
  )
  b <- chromote::ChromoteSession$new()
  withr::defer(try(b$close(), silent = TRUE), envir = envir)
  b$Page$navigate(sprintf("http://127.0.0.1:%d%s", port, path))
  e2e_wait_until(function() grepl(ready_text, e2e_text(b, ready), fixed = TRUE), what = "initial render")
  list(port = port, work_dir = work_dir, log_file = log_file, store_dir = store_dir, process = p, browser = b,
       app = app, env = env,
       logs = function() if (file.exists(log_file)) readLines(log_file) else character())
}

# Kills the app (if still alive) and starts it again on the same port.
# Returns the new process; `env` is applied over the session's own.
restart_process <- function(e, env = character()) {
  envir <- parent.frame()
  full <- e$env
  full[names(env)] <- env
  try(e$process$kill(), silent = TRUE)
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  p2 <- e2e_start_app(e$port, e$log_file, full, app = e$app, store_dir = e$store_dir)
  withr::defer(try(p2$kill(), silent = TRUE), envir = envir)
  e2e_wait_until(function() e2e_port_open(e$port), what = "app to restart")
  p2
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
  e2e_js(b, sprintf("(document.querySelector(%s) || {}).textContent || ''",
                    jsonlite::toJSON(selector, auto_unbox = TRUE)))
}

e2e_click <- function(b, selector) {
  e2e_js(b, sprintf("document.querySelector(%s).click()", jsonlite::toJSON(selector, auto_unbox = TRUE)))
  invisible()
}

e2e_set_text <- function(b, id, text) {
  e2e_js(b, sprintf(
    "(function() { var el = document.getElementById(%s); el.value = %s; $(el).trigger('change'); })()",
    jsonlite::toJSON(id, auto_unbox = TRUE), jsonlite::toJSON(text, auto_unbox = TRUE)
  ))
  invisible()
}

e2e_attr <- function(b, selector, attr) {
  e2e_js(b, sprintf(
    "(function(el) { return el ? el.getAttribute(%s) : null; })(document.querySelector(%s))",
    jsonlite::toJSON(attr, auto_unbox = TRUE), jsonlite::toJSON(selector, auto_unbox = TRUE)
  ))
}

e2e_present <- function(b, selector) {
  isTRUE(e2e_js(b, sprintf("document.querySelector(%s) !== null", jsonlite::toJSON(selector, auto_unbox = TRUE))))
}

# Marks the current document, so e2e_wait_new_page() can tell a new one from it.
e2e_mark_page <- function(b) {
  e2e_js(b, "window.__e2eOldPage = true")
  invisible()
}

e2e_wait_new_page <- function(b, timeout = 40) {
  e2e_wait_until(
    function() isTRUE(e2e_js(b, "window.__e2eOldPage === undefined && document.readyState === 'complete'")),
    timeout = timeout, what = "a new page"
  )
}

# Runs `action` (a reload by default) and waits for the new document, so a
# condition checked afterwards cannot be met by the old page. Do not run
# restart_process() inside `action`: it ties the new process to the calling
# frame, which ends when `action` returns. Use e2e_mark_page() and
# e2e_wait_new_page() around it instead.
e2e_reload <- function(b, action = function() b$Page$reload()) {
  e2e_mark_page(b)
  action()
  e2e_wait_new_page(b)
}

e2e_upload <- function(b, selector, path) {
  doc <- b$DOM$getDocument()
  node <- b$DOM$querySelector(doc$root$nodeId, selector)
  b$DOM$setFileInputFiles(files = list(normalizePath(path)), nodeId = node$nodeId)
  invisible()
}

e2e_wait_for_record <- function(store_dir, since, timeout = 20) {
  e2e_wait_until(function() {
    files <- list.files(store_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
    length(files) > 0 && any(file.mtime(files) >= since)
  }, timeout = timeout, what = "a record written after the last change")
}

e2e_records <- function(store_dir) list.files(store_dir, pattern = "\\.rds$", recursive = TRUE)

# Reads the app's stderr without blocking; call again to refresh.
e2e_stderr_reader <- function(p) {
  seen <- character()
  function() { seen <<- c(seen, p$read_error_lines()); seen }
}

e2e_dialog_choices <- function(b, id) {
  e2e_js(b, sprintf("Array.from(document.querySelectorAll('#%s button')).map(function(x) { return x.dataset.choice; }).join(',')", id))
}
