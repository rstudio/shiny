skip_if_no_e2e()

click_until <- function(b, selector, output, text) {
  e2e_click(b, selector)
  e2e_wait_until(function() grepl(text, e2e_text(b, output), fixed = TRUE), what = text)
}

set_slider <- function(b, value) {
  e2e_js(b, sprintf("(function() { var el = document.getElementById('n');
    $(el).data('shiny-input-binding').setValue(el, %d); $(el).trigger('change'); })()", value))
  e2e_wait_until(function() grepl(paste("n:", value), e2e_text(b, "#slider_val"), fixed = TRUE), what = "slider")
}

wait_for_new_process <- function(b, old_pid_text) {
  e2e_wait_until(
    function() { t <- e2e_text(b, "#process"); nzchar(t) && !identical(t, old_pid_text) },
    timeout = 40, what = "the page to reconnect to the restarted process"
  )
}

test_that("allowReconnect(TRUE) reconnects on a plain socket and keeps restoreInput()-aware UI values", {
  skip_on_cran()
  e <- e2e_session(env = c(E2E_ALLOW = "1"))
  b <- e$browser
  for (i in 1:3) click_until(b, "#plus", "#count", paste("Count:", i))
  e2e_set_text(b, "txt", "typed")
  e2e_wait_until(function() grepl("Text: typed", e2e_text(b, "#txtout"), fixed = TRUE), what = "text")
  set_slider(b, 42)
  pid1 <- e2e_text(b, "#process")

  restart_process(e)
  e2e_wait_until(function() e2e_present(b, "#shiny-reconnect-text"), what = "the reconnect notification")
  wait_for_new_process(b, pid1)
  expect_false(e2e_present(b, "#shiny-disconnected-overlay"))
  # Inputs came back; renderUI re-rendered the slider at the value the user had.
  e2e_wait_until(function() grepl("n: 42", e2e_text(b, "#slider_val"), fixed = TRUE), what = "slider value after reconnect")
  expect_identical(e2e_js(b, "document.getElementById('n').value"), "42")
  expect_match(e2e_text(b, "#txtout"), "Text: typed", fixed = TRUE)
  # Transport only: the counter observer re-ran once with the replayed button
  # value, as a reload does today. Keeping the count is enableResume()'s job.
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count"), fixed = TRUE), what = "replayed count")
  click_until(b, "#plus", "#count", "Count: 2")
})

test_that("without allowReconnect() the client does not retry", {
  skip_on_cran()
  e <- e2e_session()
  b <- e$browser
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "overlay")
  Sys.sleep(3)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
})

test_that("after ten failed attempts the notification goes and the overlay stays", {
  skip_on_cran()
  e <- e2e_session(env = c(E2E_ALLOW = "1"))
  b <- e$browser
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-reconnect-text"), what = "the reconnect notification")
  # "Try now" runs an attempt at once; each failure counts. The first
  # attempt was counted when the socket closed, so at most nine clicks.
  for (i in 1:12) {
    if (!e2e_present(b, "#shiny-reconnect-text")) break
    try(e2e_click(b, "#shiny-reconnect-now"), silent = TRUE)
    Sys.sleep(0.5)
  }
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
  expect_true(e2e_present(b, "#shiny-disconnected-overlay"))
  Sys.sleep(3)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
})

test_that("a bookmark URL is not re-restored on reconnect", {
  skip_on_cran()
  e <- e2e_session(env = c(E2E_ALLOW = "1", E2E_BOOKMARK = "1"),
                   path = "/?_inputs_&txt=%22bm%22", ready = "#txtout", ready_text = "Text: bm")
  b <- e$browser
  expect_identical(e2e_js(b, "document.getElementById('txt').value"), "bm")
  e2e_set_text(b, "txt", "live")
  e2e_wait_until(function() grepl("Text: live", e2e_text(b, "#txtout"), fixed = TRUE), what = "live text")
  pid1 <- e2e_text(b, "#process")

  restart_process(e)
  wait_for_new_process(b, pid1)
  Sys.sleep(1)   # a wrong restore would show within one flush
  expect_match(e2e_text(b, "#txtout"), "Text: live", fixed = TRUE)
  expect_identical(e2e_js(b, "document.getElementById('txt').value"), "live")
})
