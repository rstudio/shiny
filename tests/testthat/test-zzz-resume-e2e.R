skip_if_no_e2e()

e2e_session <- function(env = c(SHINY_RESUME = "TRUE")) {
  port <- httpuv::randomPort()
  store_dir <- withr::local_tempdir(.local_envir = parent.frame())
  log_file <- file.path(store_dir, "app.log")
  p <- e2e_start_app(port, store_dir, log_file, env)
  withr::defer(try(p$kill(), silent = TRUE), envir = parent.frame())
  e2e_wait_until(
    function() e2e_port_open(port),
    what = "app to start",
    on_timeout = function() p$read_all_error_lines()
  )
  b <- chromote::ChromoteSession$new()
  withr::defer(try(b$close(), silent = TRUE), envir = parent.frame())
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", port))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count")), what = "initial render")
  list(port = port, store_dir = store_dir, log_file = log_file, process = p, browser = b,
       logs = function() if (file.exists(log_file)) readLines(log_file) else character())
}

# `env` is applied over SHINY_RESUME = "TRUE".
restart_process <- function(e, env = character()) {
  full <- c(SHINY_RESUME = "TRUE")
  full[names(env)] <- env
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  p2 <- e2e_start_app(e$port, e$store_dir, e$log_file, full)
  withr::defer(try(p2$kill(), silent = TRUE), envir = parent.frame())
  e2e_wait_until(function() e2e_port_open(e$port), what = "app to restart")
  p2
}

test_that("the page continues after a process restart without recomputation, then stays live", {
  skip_on_cran()
  e <- e2e_session()
  b <- e$browser
  for (i in 1:3) {
    e2e_click(b, "#plus")
    e2e_wait_until(function() grepl(paste("Count:", i), e2e_text(b, "#count")), what = paste("count", i))
  }
  since <- Sys.time()
  e2e_click(b, "#send")
  e2e_wait_until(function() grepl("Emails sent: 1", e2e_text(b, "#emails")), what = "email")
  e2e_wait_until(function() grepl("Expensive: 10000", e2e_text(b, "#expensive")), what = "expensive")
  pid1 <- e2e_text(b, "#process")
  # The process is SIGKILLed, so there is no closing write: the debounced write must have landed.
  e2e_wait_for_snapshot(e$store_dir, since = since)
  before <- length(e$logs())
  expect_gt(before, 0)

  p2 <- restart_process(e)
  e2e_wait_until(function() e2e_js(b, "document.body.getAttribute('data-resumed') !== null"), timeout = 40, what = "shiny:resumed")
  expect_identical(e2e_js(b, "document.body.getAttribute('data-resumed')"), "snapshot:reconnect:intact")
  e2e_wait_until(function() grepl("snapshot reconnect intact", e2e_text(b, "#resumed")), what = "onResumed output")
  expect_match(e2e_text(b, "#count"), "Count: 3")
  after <- e$logs()[-seq_len(before)]
  expect_false(any(grepl("EMAIL SENT", after)))
  expect_false(any(grepl("COMPUTING", after)))
  expect_false(identical(p2$get_pid(), e$process$get_pid()))
  # The adopted output is restored, not recomputed, so it still shows the old process's PID.
  expect_identical(e2e_text(b, "#process"), pid1)

  e2e_click(b, "#plus")
  e2e_wait_until(function() grepl("Count: 4", e2e_text(b, "#count")), what = "count 4")
  e2e_js(b, "Shiny.setInputValue('n', 20)")
  e2e_wait_until(function() grepl("Expensive: 20000", e2e_text(b, "#expensive")), what = "recompute")
  expect_identical(sum(grepl("COMPUTING expensive for n=20", e$logs())), 1L)
})

test_that("a stale token falls through to inputs-only resume", {
  skip_on_cran()
  e <- e2e_session()
  b <- e$browser
  since <- Sys.time()
  e2e_click(b, "#plus")
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count")), what = "count 1")
  e2e_wait_for_snapshot(e$store_dir, since = since)
  # Expire the snapshot from the outside: a restart with a different declared
  # app version discards it.
  restart_process(e, env = c(RESUME_E2E_APP_VERSION = "v2"))
  e2e_wait_until(function() e2e_js(b, "document.body.getAttribute('data-resumed') !== null"), timeout = 40, what = "shiny:resumed")
  expect_identical(e2e_js(b, "document.body.getAttribute('data-resumed')"), "inputs:reconnect:intact")
  # #resumed is rendered by the new process, so this and the replayed count
  # reflect fresh server state rather than the pre-restart DOM text.
  e2e_wait_until(function() grepl("inputs reconnect intact", e2e_text(b, "#resumed")), what = "onResumed output")
  # Inputs replayed: the button value is 1 again, so the counter observer
  # runs once with it (today's cached-replay behaviour).
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count")), what = "replayed count")
})

test_that("a restart with a changed UI reloads the page instead of resuming into a stale one", {
  skip_on_cran()
  e <- e2e_session()
  b <- e$browser
  since <- Sys.time()
  e2e_click(b, "#plus")
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count")), what = "count 1")
  e2e_wait_for_snapshot(e$store_dir, since = since)
  restart_process(e, env = c(RESUME_E2E_EXTRA = "1"))
  e2e_wait_until(function() e2e_js(b, "document.getElementById('extra') !== null"), timeout = 40, what = "page reload with the new widget")
  # A reloaded page starts fresh in this release: no shiny:resumed, count from 0.
  expect_null(e2e_js(b, "document.body.getAttribute('data-resumed')"))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count")), what = "fresh start")
})

test_that("with resume off and no allowReconnect() call, the client does not retry", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE"))
  b <- e$browser
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_js(b, "document.getElementById('shiny-disconnected-overlay') !== null"), what = "overlay")
  Sys.sleep(3)
  expect_false(e2e_js(b, "document.getElementById('shiny-reconnect-text') !== null"))
})

test_that("with resume off, allowReconnect(TRUE) on a plain socket does not retry (main's gate)", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", RESUME_E2E_ALLOW = "TRUE"))
  b <- e$browser
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_js(b, "document.getElementById('shiny-disconnected-overlay') !== null"), what = "overlay")
  Sys.sleep(3)
  expect_false(e2e_js(b, "document.getElementById('shiny-reconnect-text') !== null"))
})

test_that("a session the server ends with an error is neither retried nor resumable", {
  skip_on_cran()
  e <- e2e_session()
  b <- e$browser
  since <- Sys.time()
  e2e_click(b, "#plus")
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count")), what = "count 1")
  e2e_wait_for_snapshot(e$store_dir, since = since)
  e2e_click(b, "#crash")
  e2e_wait_until(function() e2e_js(b, "document.getElementById('shiny-disconnected-overlay') !== null"), what = "overlay")
  snapshots <- function() list.files(e$store_dir, pattern = "\\.rds$", recursive = TRUE)
  e2e_wait_until(function() length(snapshots()) == 0, what = "the snapshot to be deleted")
  Sys.sleep(3)
  expect_false(e2e_js(b, "document.getElementById('shiny-reconnect-text') !== null"))
  expect_length(snapshots(), 0)
})
