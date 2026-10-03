skip_if_no_e2e()

click_until <- function(b, selector, output, text) {
  e2e_click(b, selector)
  e2e_wait_until(function() grepl(text, e2e_text(b, output), fixed = TRUE), what = text)
}

resumed_attr <- function(b) e2e_attr(b, "body", "data-resumed")

test_that("the page continues after a process restart: count kept, email not re-sent, expensive recomputed once, new PID", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  for (i in 1:3) click_until(b, "#plus", "#count", paste("Count:", i))
  since <- Sys.time()
  click_until(b, "#send", "#emails", "Emails sent: 1")
  e2e_wait_until(function() grepl("Expensive: 10000", e2e_text(b, "#expensive"), fixed = TRUE), what = "expensive")
  pid1 <- e2e_text(b, "#process")
  # The process is SIGKILLed, so there is no closing write: the debounced write must have landed.
  e2e_wait_for_record(e$store_dir, since = since)
  before <- length(e$logs())

  restart_process(e)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), what = "the new process id")
  expect_match(e2e_text(b, "#count"), "Count: 3", fixed = TRUE)
  after <- e$logs()[-seq_len(before)]
  expect_false(any(grepl("EMAIL SENT", after, fixed = TRUE)))
  expect_identical(sum(grepl("COMPUTING expensive for n=10", after, fixed = TRUE)), 1L)
  click_until(b, "#plus", "#count", "Count: 4")
  e2e_js(b, "Shiny.setInputValue('n', 20)")
  e2e_wait_until(function() grepl("Expensive: 20000", e2e_text(b, "#expensive"), fixed = TRUE), what = "recompute")
})

test_that("with SHINY_RESUME=FALSE, allowReconnect(TRUE) keeps PR A's behaviour: inputs replayed, nothing resumed", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"), app = "resume-e2e")
  b <- e$browser
  for (i in 1:3) click_until(b, "#plus", "#count", paste("Count:", i))
  pid1 <- e2e_text(b, "#process")
  restart_process(e)
  e2e_wait_until(function() !identical(e2e_text(b, "#process"), pid1), timeout = 40, what = "the new process id")
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count"), fixed = TRUE), what = "replayed count")
  expect_null(resumed_attr(b))
  expect_length(e2e_records(e$store_dir), 0)
})

test_that("a restart with a changed UI reloads the page and resumes without asking", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  since <- Sys.time()
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  click_until(b, "#send", "#emails", "Emails sent: 1")
  e2e_wait_for_record(e$store_dir, since = since)
  before <- length(e$logs())
  e2e_mark_page(b)
  e$process <- restart_process(e, env = c(E2E_EXTRA = "1"))
  e2e_wait_new_page(b)
  e2e_wait_until(function() e2e_present(b, "#extra"), timeout = 40, what = "the new widget")
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  expect_false(e2e_present(b, "#shiny-resume-ask"))
  e2e_wait_until(function() grepl("Count: 2", e2e_text(b, "#count"), fixed = TRUE), what = "kept count")
  expect_false(any(grepl("EMAIL SENT", e$logs()[-seq_len(before)], fixed = TRUE)))
})

test_that("a session the server ends is neither retried nor resumable", {
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
  expect_null(e2e_js(b, "sessionStorage.getItem('shiny-resume:/')"))
})

test_that("navigating away keeps the closing write (the record then expires early)", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  click_until(b, "#plus", "#count", "Count: 1")
  # Closing the tab, not navigating: headless Chrome keeps a navigated-away
  # document (and its socket) alive, so the session would not end.
  b$close()
  e2e_wait_until(function() any(grepl("SESSION ENDED", e$logs(), fixed = TRUE)), what = "the session to end")
  expect_length(e2e_records(e$store_dir), 1)
})
