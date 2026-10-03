skip_if_no_e2e()

click_until <- function(b, selector, output, text) {
  e2e_click(b, selector)
  e2e_wait_until(function() grepl(text, e2e_text(b, output), fixed = TRUE), what = text)
}
resumed_attr <- function(b) e2e_attr(b, "body", "data-resumed")

prepare <- function(e) {
  b <- e$browser
  for (i in 1:3) click_until(b, "#plus", "#count", paste("Count:", i))
  click_until(b, "#send", "#emails", "Emails sent: 1")
  since <- Sys.time()
  e2e_set_text(b, "note", "hello")
  e2e_wait_for_record(e$store_dir, since = since)
  length(e$logs())
}

test_that("under reload = 'ask' the dialog comes first; Pick up restores, Start fresh does not", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_RELOAD = "ask"), app = "resume-e2e")
  b <- e$browser
  before <- prepare(e)
  e2e_reload(b)
  e2e_wait_until(function() identical(e2e_dialog_choices(b, "shiny-resume-ask"), "pickup,fresh"), what = "the ask dialog")
  Sys.sleep(1)
  expect_identical(e2e_text(b, "#count"), "")                        # no session yet: nothing rendered
  expect_true(e2e_js(b, "document.getElementById('plus').closest('[inert]') !== null"))
  e2e_click(b, "#shiny-resume-ask button[data-choice='pickup']")
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
  e2e_wait_until(function() grepl("Count: 3", e2e_text(b, "#count"), fixed = TRUE), what = "kept count")
  expect_identical(e2e_js(b, "document.getElementById('note').value"), "hello")
  expect_false(any(grepl("EMAIL SENT", e$logs()[-seq_len(before)], fixed = TRUE)))
  expect_false(e2e_present(b, "#shiny-notification-resumed"))      # toast only under "resume"
  click_until(b, "#plus", "#count", "Count: 4")

  e2e_reload(b)
  e2e_wait_until(function() identical(e2e_dialog_choices(b, "shiny-resume-ask"), "pickup,fresh"), what = "the ask dialog")
  e2e_click(b, "#shiny-resume-ask button[data-choice='fresh']")
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "fresh start")
  expect_null(resumed_attr(b))
  expect_identical(e2e_js(b, "document.getElementById('note').value"), "")
})

test_that("under reload = 'resume' the page resumes and the toast offers Start fresh instead", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_RELOAD = "resume"), app = "resume-e2e")
  b <- e$browser
  prepare(e)
  e2e_reload(b)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
  expect_false(e2e_present(b, "#shiny-resume-ask"))
  e2e_wait_until(function() grepl("Count: 3", e2e_text(b, "#count"), fixed = TRUE), what = "kept count")
  e2e_wait_until(function() e2e_present(b, "#shiny-notification-resumed"), what = "the toast")
  expect_match(e2e_text(b, "#shiny-notification-resumed"), "Restored your session.", fixed = TRUE)
  e2e_reload(b, function() e2e_click(b, "#shiny-resume-start-fresh"))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "fresh start")
  expect_null(resumed_attr(b))
})

test_that("under reload = 'fresh' a reload starts over without asking", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE", E2E_RELOAD = "fresh"), app = "resume-e2e")
  b <- e$browser
  prepare(e)
  e2e_reload(b)
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "fresh start")
  expect_false(e2e_present(b, "#shiny-resume-ask"))
  expect_null(resumed_attr(b))
})

test_that("a function-UI reload runs ui() once, restores widgets by the push, and session$reload() is fresh", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-reload-e2e")
  b <- e$browser
  for (i in 1:2) click_until(b, "#plus", "#count", paste("Count:", i))
  e2e_set_text(b, "note", "hello")
  e2e_wait_until(function() grepl("Note: hello", e2e_text(b, "#noteout"), fixed = TRUE), what = "note")
  e2e_js(b, "(function() { var el = document.getElementById('level'); $(el).data('shiny-input-binding').setValue(el, 7); $(el).trigger('change'); })()")
  e2e_wait_until(function() grepl("Level: 7", e2e_text(b, "#levelout"), fixed = TRUE), what = "level")
  since <- Sys.time()
  path <- withr::local_tempfile(fileext = ".csv"); writeLines("hello-file", path)
  e2e_upload(b, "#file", path)
  e2e_wait_until(function() grepl(basename(path), e2e_text(b, "#fname"), fixed = TRUE), what = "upload")
  e2e_wait_for_record(e$store_dir, since = since)
  before <- length(e$logs())

  e2e_reload(b)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
  e2e_wait_until(function() grepl("Count: 2", e2e_text(b, "#count"), fixed = TRUE), what = "kept count")
  new_logs <- e$logs()[-seq_len(before)]
  expect_equal(sum(grepl("^UI GET", new_logs)), 1)
  expect_equal(sum(grepl("^SERVER START", new_logs)), 1)
  expect_identical(e2e_js(b, "document.getElementById('note').value"), "hello")
  expect_identical(e2e_js(b, "document.getElementById('level').value"), "7")
  expect_equal(e2e_js(b, "$('#plus').data('val')"), 2)
  expect_match(e2e_attr(b, "body", "data-pushed"), "note", fixed = TRUE)
  click_until(b, "#read", "#contents", "Contents: hello-file")       # the upload survived

  e2e_reload(b, function() e2e_click(b, "#reset"))
  e2e_wait_until(function() grepl("Count: 0", e2e_text(b, "#count"), fixed = TRUE), what = "reset")
  expect_null(resumed_attr(b))
})

test_that("a URL the app changed still resumes; a pasted bookmark URL wins over the stash", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-reload-e2e")
  b <- e$browser
  click_until(b, "#plus", "#count", "Count: 1")
  since <- Sys.time()
  e2e_click(b, "#setq")
  e2e_wait_until(function() identical(e2e_js(b, "location.search"), "?view=2"), what = "pushed URL")
  e2e_wait_for_record(e$store_dir, since = since)
  e2e_reload(b)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), what = "shiny:resumed")
  e2e_wait_until(function() grepl("Count: 1", e2e_text(b, "#count"), fixed = TRUE), what = "kept count")
  e2e_reload(b, function() b$Page$navigate(sprintf("http://127.0.0.1:%d/?_inputs_&note=%%22bm%%22", e$port)))
  e2e_wait_until(function() grepl("Note: bm", e2e_text(b, "#noteout"), fixed = TRUE), what = "bookmark restore")
  expect_null(resumed_attr(b))
  expect_match(e2e_text(b, "#count"), "Count: 0", fixed = TRUE)
})

test_that("after a process restart, a reload restores an upload whose file is readable", {
  skip_on_cran()
  e <- e2e_session(env = c(SHINY_RESUME = "TRUE"), app = "resume-reload-e2e")
  b <- e$browser
  since <- Sys.time()
  path <- withr::local_tempfile(fileext = ".csv"); writeLines("survives", path)
  e2e_upload(b, "#file", path)
  e2e_wait_until(function() grepl(basename(path), e2e_text(b, "#fname"), fixed = TRUE), what = "upload")
  e2e_wait_for_record(e$store_dir, since = since)
  e$process <- restart_process(e)
  e2e_reload(b)
  e2e_wait_until(function() identical(resumed_attr(b), "snapshot"), timeout = 40, what = "shiny:resumed")
  click_until(b, "#read", "#contents", "Contents: survives")
  expect_match(e2e_text(b, "#contents"), "Contents: survives", fixed = TRUE)
})
