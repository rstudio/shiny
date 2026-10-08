withr::local_envvar(SHINY_RESUME = "TRUE")

# The wire after the client change (spec §2.1): the payload is the input set
# as of the disconnect; what the user did while disconnected follows as
# `update`s. Every case runs in both timings and on both paths.
both_timings <- function(f) for (before in c(TRUE, FALSE)) f(before)

test_that("a queued click fires once on the inputs-only path, in both timings", {
  local_snapshot_store_dir()
  both_timings(function(before) {
    spy <- new_spy("obs", "txt")
    res <- suppressMessages(resume_with_updates(
      counter_app(spy),
      list(inputs = list(`plus:shiny.action` = 3L, txt = "typed")),
      updates = list(list(`plus:shiny.action` = 4L)),
      updates_before_flush = before
    ))
    expect_identical(res$outcome, "inputs")
    expect_equal(spy$obs, 1, info = paste("before =", before))
    expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
  })
})

test_that("a queued click fires once on the snapshot path, in both timings", {
  local_snapshot_store_dir()
  both_timings(function(before) {
    spy <- new_spy("obs", "txt")
    token <- closed_counter_session()
    res <- suppressMessages(resume_with_updates(
      counter_app(spy),
      list(token = token, inputs = list(`plus:shiny.action` = 3L, txt = "typed")),
      updates = list(list(`plus:shiny.action` = 4L)),
      updates_before_flush = before
    ))
    expect_identical(res$outcome, "snapshot")
    expect_equal(spy$obs, 1, info = paste("before =", before))
    expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
  })
})

test_that("a queued update processed before the first flush lands after the hold-back (deferral pin)", {
  local_snapshot_store_dir()
  spy <- new_spy("obs", "txt")
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  server <- counter_app(spy)
  run <- function() withReactiveDomain(s, isolate(server(input = s$input, output = s$output, session = s)))
  suppressMessages(s$.resumeSession(list(inputs = list(`plus:shiny.action` = 3L)), run))
  s$manageInputs(withReactiveDomain(s, applyInputHandlers(list(`plus:shiny.action` = 4L), s)))
  # Not applied yet: the server function's observers keep busyCount > 0.
  expect_equal(as.numeric(isolate(s$input$plus)), 3)
  flushReact(); s$flushOutput()
  expect_true(!isTRUE(s$.resumeHoldBack))
  expect_equal(spy$obs, 0)                      # first run held back
  drain_later(); flushReact(); s$flushOutput()
  expect_equal(as.numeric(isolate(s$input$plus)), 4)
  expect_equal(spy$obs, 1)                      # the update, after the hold-back
})

test_that("ignoreInit = TRUE fires once for a queued value change, on both paths", {
  local_snapshot_store_dir()
  app <- function(spy) { force(spy); function(input, output, session) {
    observeEvent(input$txt, spy$ignore <- spy$ignore + 1, ignoreInit = TRUE)
  }}
  both_timings(function(before) {
    spy <- new_spy("ignore")
    res <- suppressMessages(resume_with_updates(
      app(spy), list(inputs = list(txt = "typed")),
      updates = list(list(txt = "typed more")), updates_before_flush = before
    ))
    expect_equal(spy$ignore, 1, info = paste("inputs-only, before =", before))
  })
  both_timings(function(before) {
    spy <- new_spy("ignore")
    live <- live_session(app(spy), inputs = list(txt = "typed"))
    token <- live_close(live)
    res <- suppressMessages(resume_with_updates(
      app(spy), list(token = token, inputs = list(txt = "typed")),
      updates = list(list(txt = "typed more")), updates_before_flush = before
    ))
    expect_identical(res$outcome, "snapshot")
    expect_equal(spy$ignore, 1, info = paste("snapshot, before =", before))
  })
})

test_that("several queued updates to one input arrive in order after the restored value", {
  local_snapshot_store_dir()
  seen <- character(0)
  app <- function(input, output, session) observe(seen <<- c(seen, input$txt))
  live <- live_session(app, inputs = list(txt = "first"))
  token <- live_close(live)
  seen <- character(0)
  suppressMessages(resume_with_updates(
    app, list(token = token, inputs = list(txt = "first")),
    updates = list(list(txt = "a"), list(txt = "ab"), list(txt = "abc"))
  ))
  expect_identical(seen, c("first", "a", "ab", "abc"))
})
