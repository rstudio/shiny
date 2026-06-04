# Browser-driven tests for the client side of session$close(hard = TRUE):
# the #shiny-closed-overlay, the shiny:closed jQuery event, and the
# soft-close regression check.
#
# The R unit tests in test-hard-disconnect.R cover the wire-format and
# server-state contracts; this file covers the parts that only happen
# in a real browser.
#
# Unlike test-zzz-st2-download.R, we cannot share one launched app across
# all test_that blocks: each scenario closes the Shiny session as its
# action under test, so subsequent tests need a fresh session. We launch
# the app inside each test_that. The function body is defined once below
# and reused.

skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
test_app_fn <- function() {
  library(shiny)

  ui <- fluidPage(
    actionButton("close_hard", "Close hard now"),
    actionButton("close_soft", "Close soft"),
    actionButton("close_app_default", "Close hard (app default)"),
    actionButton("submit", "Submit and end session"),
    textInput("name", "Your name", "")
  )

  server <- function(input, output, session) {
    observeEvent(input$close_hard, {
      session$close(
        hard = TRUE,
        message = "Thanks for testing the hard-disconnect feature."
      )
    })
    observeEvent(input$close_soft, {
      session$close()
    })
    observeEvent(input$close_app_default, {
      session$close(hard = TRUE)
    })
    observeEvent(input$submit, {
      nm <- trimws(input$name)
      greeting <- if (nzchar(nm)) {
        paste0("Thanks, ", nm, "!")
      } else {
        "Thanks!"
      }
      showModal(modalDialog(
        title = "Submission received",
        greeting,
        footer = NULL,
        easyClose = FALSE
      ))
      later::later(
        ~ session$close(hard = TRUE, message = paste0(greeting, " Done.")),
        delay = 1
      )
    })
  }

  shinyApp(
    ui, server,
    hardDisconnectMessage = "App default text"
  )
}

# Helper: launch a fresh app and install the jQuery event recorder.
# shiny:closed is dispatched via $(document).trigger(...), which only
# reaches jQuery .on() handlers — native document.addEventListener
# does NOT receive it. This matches the rest of Shiny's event surface.
launch_close_app <- function(envir = parent.frame()) {
  app <- launch_test_app(test_app_fn, envir = envir)
  app$run_js(
    "
    window.__shinyEvents = [];
    $(document).on('shiny:closed', function(e) {
      var detail = e.detail || (e.originalEvent && e.originalEvent.detail);
      window.__shinyEvents.push({ type: 'shiny:closed', detail: detail });
    });
    $(document).on('shiny:disconnected', function() {
      window.__shinyEvents.push({ type: 'shiny:disconnected' });
    });
    "
  )
  app
}

test_that("close_hard renders #shiny-closed-overlay with the per-call message", {
  app <- launch_close_app()

  not_yet <- app$get_js("document.querySelectorAll('#shiny-closed-overlay').length")
  expect_equal(not_yet, 0)

  app$click("close_hard", wait_ = FALSE)
  app$wait_for_js("document.querySelectorAll('#shiny-closed-overlay').length === 1")

  overlay_text <- app$get_js("document.querySelector('#shiny-closed-overlay').textContent")
  expect_equal(overlay_text, "Thanks for testing the hard-disconnect feature.")

  disconnected_count <- app$get_js(
    "document.querySelectorAll('#shiny-disconnected-overlay').length"
  )
  expect_equal(disconnected_count, 0)

  closed_msg <- app$get_js(
    "(function() {
       var ev = window.__shinyEvents.find(function(e) { return e.type === 'shiny:closed'; });
       return ev && ev.detail && ev.detail.message;
     })()"
  )
  expect_equal(closed_msg, "Thanks for testing the hard-disconnect feature.")
})

test_that("close_hard with no per-call message falls back to shinyApp(hardDisconnectMessage=)", {
  app <- launch_close_app()

  app$click("close_app_default", wait_ = FALSE)
  app$wait_for_js("document.querySelectorAll('#shiny-closed-overlay').length === 1")

  overlay_text <- app$get_js("document.querySelector('#shiny-closed-overlay').textContent")
  expect_equal(overlay_text, "App default text")
})

test_that("close_soft preserves today's behavior: grey overlay, no closed overlay, no shiny:closed", {
  app <- launch_close_app()

  app$click("close_soft", wait_ = FALSE)
  app$wait_for_js("document.querySelectorAll('#shiny-disconnected-overlay').length === 1")

  closed_count <- app$get_js(
    "document.querySelectorAll('#shiny-closed-overlay').length"
  )
  expect_equal(closed_count, 0)

  saw_closed <- app$get_js(
    "window.__shinyEvents.some(function(e) { return e.type === 'shiny:closed'; })"
  )
  expect_false(isTRUE(saw_closed))

  saw_disconnected <- app$get_js(
    "window.__shinyEvents.some(function(e) { return e.type === 'shiny:disconnected'; })"
  )
  expect_true(isTRUE(saw_disconnected))
})

test_that("submit-and-exit pattern: modal first, then closed overlay with combined message", {
  app <- launch_close_app()

  app$set_inputs(name = "Liz")
  app$click("submit", wait_ = FALSE)

  app$wait_for_js("document.querySelectorAll('.modal-body').length === 1")
  modal_text <- app$get_js("document.querySelector('.modal-body').textContent")
  expect_match(modal_text, "Thanks, Liz!")

  app$wait_for_js(
    "document.querySelectorAll('#shiny-closed-overlay').length === 1",
    timeout = 5000
  )
  overlay_text <- app$get_js("document.querySelector('#shiny-closed-overlay').textContent")
  expect_match(overlay_text, "Thanks, Liz!")
  expect_match(overlay_text, "Done\\.")
})
