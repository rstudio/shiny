skip_if_no_e2e()

click_until <- function(b, selector, output, text) {
  e2e_click(b, selector)
  e2e_wait_until(function() grepl(text, e2e_text(b, output), fixed = TRUE), what = text)
}
resumed_attr <- function(b) e2e_attr(b, "body", "data-resumed")

# Clicks plus `n` times, waits for the record, then makes `risky` fail.
crash_after_saving <- function(e, n = 1) {
  b <- e$browser
  since <- Sys.time()
  for (i in seq_len(n)) click_until(b, "#plus", "#count", paste("Count:", i))
  e2e_wait_for_record(e$store_dir, since = since)
  e2e_set_text(b, "risky", "boom")
}

wait_for_dialog <- function(b, choices) {
  e2e_wait_until(function() identical(e2e_dialog_choices(b, "shiny-fatal-error"), choices), what = paste("dialog", choices))
}

test_that("Resume lands on the pre-crash state and the crashing input is not replayed", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_RELOAD = "ask"), app = "resume-e2e")
  b <- e$browser
  err <- e2e_stderr_reader(e$process)
  crash_after_saving(e, n = 3)
  wait_for_dialog(b, "resume,fresh")
  expect_match(e2e_text(b, "#shiny-fatal-error code"), "risky boom", fixed = TRUE)
  expect_match(e2e_text(b, "#shiny-fatal-error p"), "Resume returns to the state saved just before the error", fixed = TRUE)
  expect_true(e2e_present(b, "#shiny-disconnected-overlay"))
  expect_true(e2e_js(b, "document.getElementById('plus').closest('[inert]') !== null"))
  Sys.sleep(2)
  expect_false(e2e_present(b, "#shiny-reconnect-text"))             # no retry behind the dialog
  e2e_reload(b, function() e2e_click(b, "#shiny-fatal-error button[data-choice='resume']"))
  expect_false(e2e_present(b, "#shiny-resume-ask"))                 # no second ask
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
  e2e_wait_until(function() grepl("Count: 3", e2e_text(b, "#count"), fixed = TRUE), what = "saved count")
  expect_identical(e2e_js(b, "document.getElementById('risky').value"), "")
  expect_true(any(grepl("Resumed (reload): restored", err(), fixed = TRUE)))
  click_until(b, "#plus", "#count", "Count: 4")
})

test_that("before any write only Start over is offered, and it starts a new session", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  e2e_set_text(b, "risky", "boom")
  wait_for_dialog(b, "fresh")
  e2e_reload(b, function() e2e_click(b, "#shiny-fatal-error button[data-choice='fresh']"))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "fresh start")
  expect_null(resumed_attr(b))
})

test_that("a dropped socket while the dialog is open neither retries nor removes it", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-e2e")
  b <- e$browser
  crash_after_saving(e)
  wait_for_dialog(b, "resume,fresh")
  e$process$kill()
  Sys.sleep(3)
  expect_identical(e2e_dialog_choices(b, "shiny-fatal-error"), "resume,fresh")
  expect_false(e2e_present(b, "#shiny-reconnect-text"))
})

test_that("error text with markup is shown as text", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_ERROR_TEXT = "<img id=injected src=x>"), app = "resume-e2e")
  b <- e$browser
  crash_after_saving(e)
  wait_for_dialog(b, "resume,fresh")
  expect_identical(e2e_text(b, "#shiny-fatal-error code"), "<img id=injected src=x>")
  expect_false(e2e_present(b, "#injected"))
})

test_that("a resumed page that fails twice within seconds loads fresh the third time", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_RELOAD = "resume"), app = "resume-e2e")
  b <- e$browser
  failures <- function() e2e_js(b, "(JSON.parse(sessionStorage.getItem('shiny-resume:/')) || {}).failures")
  crash_after_saving(e, n = 2)
  wait_for_dialog(b, "resume,fresh")
  for (attempt in 1:2) {
    e2e_reload(b, function() e2e_click(b, "#shiny-fatal-error button[data-choice='resume']"))
    e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
    e2e_set_text(b, "risky", "boom")                               # within ten seconds of the resume
    wait_for_dialog(b, "resume,fresh")
    e2e_wait_until(function() identical(failures(), attempt), what = paste("failure", attempt))
  }
  e2e_reload(b, function() e2e_click(b, "#shiny-fatal-error button[data-choice='resume']"))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "fresh start")
  expect_null(resumed_attr(b))
  expect_identical(failures(), 0L)                                   # the new session's stash starts clean
})
