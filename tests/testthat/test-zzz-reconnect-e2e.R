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
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"))
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
  # The replayed button count is an event that already happened, so the
  # counter's handler is held back; the next click counts.
  Sys.sleep(1)
  expect_match(e2e_text(b, "#count"), "Count: 0", fixed = TRUE)
  click_until(b, "#plus", "#count", "Count: 1")
})

test_that("without allowReconnect() the client does not retry", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE"))
  b <- e$browser
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "overlay")
  Sys.sleep(3)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
})

test_that("retries continue while the server is down and reconnect when it returns", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"))
  b <- e$browser
  pid1 <- e2e_text(b, "#process")
  e$process$kill()
  e2e_wait_until(function() !e2e_port_open(e$port), what = "port to close")
  e2e_wait_until(function() e2e_present(b, "#shiny-reconnect-text"), what = "the reconnect notification")
  Sys.sleep(8)                                   # several failed attempts
  expect_true(e2e_present(b, "#shiny-reconnect-text"))
  expect_true(e2e_present(b, "#shiny-reconnect-now"))
  expect_true(e2e_present(b, "#shiny-disconnected-overlay"))
  restart_process(e)
  wait_for_new_process(b, pid1)
  expect_false(e2e_present(b, "#shiny-disconnected-overlay"))
})

test_that("a bookmark URL is not re-restored on reconnect", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1", E2E_BOOKMARK = "1"),
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

test_that("a connection layer that vetoes reconnecting (a clean server close) is honoured", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "FALSE", E2E_ALLOW = "1"))
  b <- e$browser
  # What shiny-server-client does after a clean close such as an idle timeout.
  e2e_js(b, "Shiny.shinyapp.$socket.allowReconnect = false; true")
  e$process$kill()
  e2e_wait_until(function() e2e_present(b, "#shiny-disconnected-overlay"), what = "the overlay")
  Sys.sleep(4)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
})
