skip_if_no_e2e()

click_until <- function(b, selector, output, text) {
  e2e_click(b, selector)
  e2e_wait_until(function() grepl(text, e2e_text(b, output), fixed = TRUE), what = text)
}

resumed_attr <- function(b) e2e_attr(b, "body", "data-resumed")

new_logs <- function(e, before) e$logs()[-seq_len(before)]

test_that("the page continues after a process restart: count kept, email not re-sent, expensive recomputed once, new PID", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  for (i in 1:3) click_until(b, "#plus", "#count", paste("Count:", i))
  since <- Sys.time()
  click_until(b, "#send", "#emails", "Emails sent: 1")
  e2e_wait_until(function() grepl("Expensive: 10000", e2e_text(b, "#expensive"), fixed = TRUE), what = "expensive")
  pid1 <- e2e_text(b, "#process")
  # SIGKILL: no closing write, so the debounced write must have landed.
  e2e_wait_for_record(e$store_dir, since = since)
  before <- length(e$logs())

  restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), what = "the new process id")
  expect_match(e2e_text(b, "#count"), "Count: 3", fixed = TRUE)
  expect_match(e2e_text(b, "#emails"), "Emails sent: 1", fixed = TRUE)
  after <- new_logs(e, before)
  expect_false(any(grepl("EMAIL SENT", after, fixed = TRUE)))
  expect_identical(sum(grepl("COMPUTING expensive for n=10", after, fixed = TRUE)), 1L)
  click_until(b, "#plus", "#count", "Count: 4")
})

test_that("a dropped socket on a live process reconnects into saved state without re-sending the email", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  click_until(b, "#send", "#emails", "Emails sent: 1")
  pid1 <- e2e_text(b, "#process")
  before <- length(e$logs())
  # The server's closing write saves the state; the client retries at once.
  e2e_js(b, "Shiny.shinyapp.$socket.close()")
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  expect_identical(e2e_text(b, "#process"), pid1)
  expect_match(e2e_text(b, "#count"), "Count: 2", fixed = TRUE)
  expect_false(any(grepl("EMAIL SENT", new_logs(e, before), fixed = TRUE)))
})

# Spec §1.0: the overlay does not block input, and the client used to send
# every outage-era change twice (in the `resume` payload and as a queued
# `update`), so the server saw `abc, a, ab, abc` and could fire a click twice.
outage_changes_case <- function(env, emails_after) {
  e <- e2e_session(env = env, app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  click_until(b, "#send", "#emails", "Emails sent: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "overlay")
  before <- length(e$logs())
  for (w in c("a", "ab", "abc")) { e2e_set_text(b, "word", w); Sys.sleep(0.2) }
  e2e_click(b, "#send")
  restart_process(e)
  e2e_wait_until(function() !is.null(resumed_attr(b)), timeout = 60, what = "shiny:resumed")
  e2e_wait_until(function() grepl(emails_after, e2e_text(b, "#emails"), fixed = TRUE), what = emails_after)
  Sys.sleep(1)                                   # a duplicate would show within one flush
  after <- new_logs(e, before)
  expect_identical(sum(grepl("EMAIL SENT", after, fixed = TRUE)), 1L)
  words <- sub("^WORD ", "", grep("^WORD ", after, value = TRUE))
  expect_identical(words, c("first", "a", "ab", "abc"))
}

test_that("changes made while disconnected arrive once, in order, after the restored state (snapshot)", {
  skip_on_cran()
  outage_changes_case(c(SHINY_RESUME = "TRUE"), "Emails sent: 2")
})

test_that("changes made while disconnected arrive once, in order, after the restored state (inputs only)", {
  skip_on_cran()
  outage_changes_case(c(SHINY_RESUME = "TRUE", E2E_BLOCK = "1"), "Emails sent: 1")
})

test_that("with SHINY_RESUME=FALSE a reconnect replays inputs but no events, and nothing is saved", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"), app = "resume-e2e")
  b <- e$browser
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  click_until(b, "#send", "#emails", "Emails sent: 1")
  pid1 <- e2e_text(b, "#process")
  before <- length(e$logs())
  restart_process(e)
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), timeout = 40, what = "the new process id")
  Sys.sleep(1)                                   # a replayed event would show within one flush
  expect_match(e2e_text(b, "#count"), "Count: 0", fixed = TRUE)
  expect_false(any(grepl("EMAIL SENT", new_logs(e, before), fixed = TRUE)))
  expect_null(resumed_attr(b))
  expect_length(e2e_records(e$store_dir), 0)
  click_until(b, "#plus", "#count", "Count: 1")
})

test_that("a page keeps retrying through a long outage and resumes with the token it kept", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  e2e_wait_for_record(e$store_dir, since = since)
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-reconnect-text"), what = "the reconnect notification")
  Sys.sleep(8)
  expect_true(e2e_present(b, "#shiny-reconnect-text"))
  restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 60, what = "shiny:resumed")
  expect_match(e2e_text(b, "#count"), "Count: 2", fixed = TRUE)
})

test_that("a record that cannot be complete resumes from inputs only: count 0, no email, cause logged", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_BLOCK = "1"), app = "resume-e2e")
  b <- e$browser
  stderr <- e2e_stderr_reader(e$process)
  since <- Sys.time()
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  click_until(b, "#send", "#emails", "Emails sent: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  before <- length(e$logs())
  p2 <- restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "inputs"), timeout = 40, what = "shiny:resumed")
  Sys.sleep(1)
  expect_match(e2e_text(b, "#count"), "Count: 0", fixed = TRUE)
  expect_false(any(grepl("EMAIL SENT", new_logs(e, before), fixed = TRUE)))
  stderr2 <- e2e_stderr_reader(p2)
  e2e_wait_until(function() any(grepl("Resumed from inputs only: saved state incomplete: |reactiveVal|conn", stderr2(), fixed = TRUE)),
                 what = "the inputs-only log line")
})

test_that("a fatal observer error ends the session for good: no retry, record deleted, a reload starts fresh", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  click_until(b, "#plus", "#count", "Count: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  e2e_set_text(b, "risky", "boom")
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "overlay")
  e2e_wait_until(function() length(e2e_records(e$store_dir)) == 0, what = "the record to be deleted")
  Sys.sleep(3)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
  e2e_reload(b)
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "a fresh page")
  expect_null(resumed_attr(b))
})

test_that("a session the server closes is neither retried nor resumable", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  click_until(b, "#plus", "#count", "Count: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  e2e_click(b, "#end")
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "overlay")
  e2e_wait_until(function() length(e2e_records(e$store_dir)) == 0, what = "the record to be deleted")
  Sys.sleep(3)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
  expect_identical(e2e_js(b, "sessionStorage.length"), 0L)
})

test_that("closing the tab deletes the record", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  click_until(b, "#plus", "#count", "Count: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  b$close()
  e2e_wait_until(function() any(grepl("SESSION ENDED", e$logs(), fixed = TRUE)), what = "the session to end")
  e2e_wait_until(function() length(e2e_records(e$store_dir)) == 0, what = "the record to be deleted")
})

test_that("an uploaded file survives a restart", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  path <- withr::local_tempfile(fileext = ".csv"); writeLines("survives", path)
  since <- Sys.time()
  e2e_upload(b, "#file", path)
  e2e_wait_for_record(e$store_dir, since = since)
  restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  click_until(b, "#read", "#contents", "Contents: survives")
})

test_that("panels added by clicks come back live after a restart, rebuilt by onResume()", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  e2e_click(b, "#add")
  e2e_wait_until(function() e2e_present(b, "#p1-count"), what = "panel p1")
  e2e_click(b, "#add")
  e2e_wait_until(function() e2e_present(b, "#p2-count"), what = "panel p2")
  since <- Sys.time()
  click_until(b, "#p1-plus", "#p1-count", "Panel count: 1")
  pid1 <- e2e_text(b, "#process")
  e2e_wait_for_record(e$store_dir, since = since)

  restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), what = "the new process id")
  expect_equal(e2e_js(b, "document.querySelectorAll('#panels .panel').length"), 2)
  click_until(b, "#p1-plus", "#p1-count", "Panel count: 2")
  click_until(b, "#p2-plus", "#p2-count", "Panel count: 1")
})

test_that("with resume off, onResume() redefines an output a handler built, and it updates after a reconnect", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"), app = "resume-e2e")
  b <- e$browser
  click_until(b, "#build", "#built", "Built: first")
  pid1 <- e2e_text(b, "#process")
  restart_process(e)
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), timeout = 40, what = "the new process id")
  e2e_set_text(b, "word", "second")
  e2e_wait_until(function() grepl("Built: second", e2e_text(b, "#built"), fixed = TRUE), what = "the rebuilt output")
})

test_that("saved state that no longer fits ends the session with a notice and a Reload link", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_APP_VERSION = "1"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  click_until(b, "#plus", "#count", "Count: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  p2 <- restart_process(e, env = c(E2E_EXTRA_SOURCE = "1"))
  stderr <- e2e_stderr_reader(p2)
  e2e_wait_until(function() e2e_present(b, "#shiny-resume-ended"), timeout = 40, what = "the resume-ended notice")
  expect_true(e2e_present(b, "#shiny-reload-link"))
  expect_true(e2e_present(b, "#shiny-disconnected-overlay"))
  Sys.sleep(4)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
  e2e_wait_until(function() any(grepl("Resume failed after the server function ran", stderr())), what = "the resume failure in the server log")
})
