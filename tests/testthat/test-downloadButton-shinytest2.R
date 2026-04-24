skip_on_cran()
# Skip on everything but mac
skip_on_os(c("windows", "linux", "solaris", "emscripten"))
skip_if_not_installed("shinytest2")
skip_if_not_installed("callr")
library(shinytest2)

# Start the test app in a subprocess, loading the local dev shiny.
# AppDriver$new(url) is used instead of AppDriver$new(app_dir) because the
# latter's internal subprocess runner has a timing issue with devtools-loaded
# packages that prevents the shinytest2 tracer from detecting jQuery.
app_process <- callr::r_bg(
  function(app_file, port) {
    devtools::load_all(quiet = TRUE)
    shiny::runApp(
      shiny::shinyAppFile(app_file),
      port = port,
      host = "127.0.0.1",
      launch.browser = FALSE,
      quiet = TRUE
    )
  },
  args = list(
    app_file = testthat::test_path("apps/download-button/app.R"),
    port = 7314L
  )
)
withr::defer(app_process$kill())

# Wait for the app to be ready
for (i in seq_len(20)) {
  Sys.sleep(0.5)
  ready <- tryCatch(
    {
      httr::GET("http://127.0.0.1:7314", httr::timeout(1))
      TRUE
    },
    error = function(e) FALSE
  )
  if (ready) break
}
if (!ready) skip("Download button test app failed to start")

app_url <- "http://127.0.0.1:7314"

# Start up app once and share across all tests
app <- AppDriver$new(app_url)
withr::defer(app$stop())
app$wait_for_idle()

is_disabled <- function(id) {
  app$get_js(sprintf(
    "var el = document.querySelector('#%s');
     el.classList.contains('disabled') &&
     el.getAttribute('aria-disabled') === 'true' &&
     el.getAttribute('tabindex') === '-1';", id
  ))
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

click_toggle <- function(id) {
  app$click(input = paste0("toggle_", id), wait_ = FALSE)
  app$wait_for_idle()
}

# ---------------------------------------------------------------------------
# downloadButton
# ---------------------------------------------------------------------------

test_that("downloadButton (enabled='auto') auto-enables after server init", {
  expect_false(is_disabled("btn_auto"))
  expect_null(app$get_js("document.querySelector('#btn_auto').getAttribute('aria-disabled')"))
})

test_that("downloadButton (enabled=FALSE) stays disabled after server init", {
  expect_true(is_disabled("btn_off"))
})

test_that("downloadButton (enabled=TRUE) starts and stays enabled", {
  expect_false(is_disabled("btn_on"))
  expect_null(app$get_js("document.querySelector('#btn_on').getAttribute('aria-disabled')"))
})

test_that("downloadButton with shinyjs-disabled class stays disabled after server init", {
  expect_true(is_disabled("btn_shinyjs"))
})

test_that("downloadButton (enabled='auto') can be toggled off and back on", {
  click_toggle("btn_auto")
  expect_true(is_disabled("btn_auto"))

  click_toggle("btn_auto")
  expect_false(is_disabled("btn_auto"))
})

test_that("downloadButton (enabled=FALSE) can be toggled on and back off", {
  click_toggle("btn_off")
  expect_false(is_disabled("btn_off"))

  click_toggle("btn_off")
  expect_true(is_disabled("btn_off"))
})

test_that("downloadButton (enabled=TRUE) can be toggled off and back on", {
  click_toggle("btn_on")
  expect_true(is_disabled("btn_on"))

  click_toggle("btn_on")
  expect_false(is_disabled("btn_on"))
})

test_that("downloadButton (shinyjs-disabled) can be toggled on and back off", {
  click_toggle("btn_shinyjs")
  expect_false(is_disabled("btn_shinyjs"))

  click_toggle("btn_shinyjs")
  expect_true(is_disabled("btn_shinyjs"))
})

# ---------------------------------------------------------------------------
# downloadLink
# ---------------------------------------------------------------------------

test_that("downloadLink (enabled='auto') auto-enables after server init", {
  expect_false(is_disabled("lnk_auto"))
  expect_null(app$get_js("document.querySelector('#lnk_auto').getAttribute('aria-disabled')"))
})

test_that("downloadLink (enabled=FALSE) stays disabled after server init", {
  expect_true(is_disabled("lnk_off"))
})

test_that("downloadLink (enabled=TRUE) starts and stays enabled", {
  expect_false(is_disabled("lnk_on"))
  expect_null(app$get_js("document.querySelector('#lnk_on').getAttribute('aria-disabled')"))
})

test_that("downloadLink with shinyjs-disabled class stays disabled after server init", {
  expect_true(is_disabled("lnk_shinyjs"))
})

test_that("downloadLink (enabled='auto') can be toggled off and back on", {
  click_toggle("lnk_auto")
  expect_true(is_disabled("lnk_auto"))

  click_toggle("lnk_auto")
  expect_false(is_disabled("lnk_auto"))
})

test_that("downloadLink (enabled=FALSE) can be toggled on and back off", {
  click_toggle("lnk_off")
  expect_false(is_disabled("lnk_off"))

  click_toggle("lnk_off")
  expect_true(is_disabled("lnk_off"))
})

test_that("downloadLink (enabled=TRUE) can be toggled off and back on", {
  click_toggle("lnk_on")
  expect_true(is_disabled("lnk_on"))

  click_toggle("lnk_on")
  expect_false(is_disabled("lnk_on"))
})

test_that("downloadLink (shinyjs-disabled) can be toggled on and back off", {
  click_toggle("lnk_shinyjs")
  expect_false(is_disabled("lnk_shinyjs"))

  click_toggle("lnk_shinyjs")
  expect_true(is_disabled("lnk_shinyjs"))
})
