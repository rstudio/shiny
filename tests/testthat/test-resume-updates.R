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

# A session whose record says plus = 2 while the old session got to 3 and
# died inside the debounce. Returns the token.
record_lag_session <- function(spy) {
  live <- live_session(counter_app(spy), outputs = "n", inputs = list(`plus:shiny.action` = 0, txt = ""),
                       envir = parent.frame())
  for (i in 1:2) live_set(live$session, `plus:shiny.action` = i)
  live$session$.snapshotRegistry$writeNow()            # the record says 2 ...
  live_set(live$session, `plus:shiny.action` = 3)      # ... the old session got to 3 and died inside the debounce
  token <- live$ws$sent()[[1]]$config$resumeToken
  # A closed gate keeps the closing write from refreshing the record, and a
  # resolvable store lets the cleanup in local_cancel_writes() delete it.
  live$session$.snapshotRegistry$configureWriter(function() snapshot_store(), snapshot_record_key(token), function() FALSE, 1000)
  live$session$wsClosed()
  token
}

test_that("record lag: a payload value newer than the record is restored as state and a queued update still fires once", {
  local_snapshot_store_dir()
  both_timings(function(before) {
    token <- record_lag_session(new_spy("obs", "txt"))
    spy <- new_spy("obs", "txt")
    res <- suppressMessages(resume_with_updates(
      counter_app(spy),
      list(token = token, inputs = list(`plus:shiny.action` = 3L, txt = "", .clientdata_output_n_hidden = FALSE)),
      updates = list(list(`plus:shiny.action` = 4L)),
      updates_before_flush = before
    ))
    expect_identical(res$outcome, "snapshot")
    expect_equal(spy$obs, 1, info = paste("before =", before))   # the queued click only
    expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
    values <- sent_of(res$ws, "values")
    expect_identical(values[[1]]$values$n, "2")                   # the record's count at the first flush
    expect_identical(values[[length(values)]]$values$n, "3")      # then the queued click
  })
})

test_that("at most once: a payload value with no queued update never fires its handler", {
  local_snapshot_store_dir()
  token <- record_lag_session(new_spy("obs", "txt"))
  spy <- new_spy("obs", "txt")
  res <- suppressMessages(resume_with_updates(
    counter_app(spy), list(token = token, inputs = list(`plus:shiny.action` = 3L, txt = ""))
  ))
  expect_identical(res$outcome, "snapshot")
  expect_equal(spy$obs, 0)
  expect_equal(as.numeric(isolate(res$session$input$plus)), 3)
})

test_that("the record written after the first flush contains the merged inputs", {
  local_snapshot_store_dir()
  token <- record_lag_session(new_spy("obs", "txt"))
  res <- suppressMessages(resume_with_updates(
    counter_app(new_spy("obs", "txt")), list(token = token, inputs = list(`plus:shiny.action` = 3L, txt = ""))
  ))
  newToken <- res$ws$sent()[[1]]$config$resumeToken
  rec <- snapshot_store()$read(snapshot_record_key(newToken))
  expect_equal(as.numeric(rec$inputs$plus), 3)
})

test_that("a construction-time guard reads the record, not the payload; the payload converges afterwards", {
  local_snapshot_store_dir()
  app <- function(input, output, session) {
    if (isTRUE(input$mode == "b")) extra <- reactiveVal("only in b")
    observe({ input$mode; NULL })
  }
  live <- live_session(app, inputs = list(mode = "a"))
  token <- live_close(live)
  res <- suppressMessages(resume_with_updates(app, list(token = token, inputs = list(mode = "b"))))
  expect_identical(res$outcome, "snapshot")                  # the gate passed: construction saw "a"
  expect_identical(isolate(res$session$input$mode), "b")     # then the payload value won
})

test_that("payload clientData equal to the session's initial value but not the record's still invalidates", {
  local_snapshot_store_dir()
  seen <- character(0)
  app <- function(input, output, session) observe(seen <<- c(seen, session$clientData$url_search))
  live <- live_session(app, inputs = list(.clientdata_url_search = "?v=old"))
  token <- live_close(live)
  seen <- character(0)
  res <- suppressMessages(resume_with_updates(app, list(token = token, inputs = list(.clientdata_url_search = ""))))
  expect_identical(seen, "")                                 # one run, with the live value
})

test_that("an input the server updates during the restore keeps the client's echo, not the payload", {
  local_snapshot_store_dir()
  app <- function(input, output, session) {
    observe({ input$txt; NULL })
    updateTextInput(session, "txt", value = "from server")
  }
  live <- live_session(app, inputs = list(txt = "old"))
  live$session$.snapshotRegistry$writeNow()
  live_set(live$session, txt = "typed")                # record lag: the record says "old"
  token <- live$ws$sent()[[1]]$config$resumeToken
  live$session$.snapshotRegistry$configureWriter(function() snapshot_store(), snapshot_record_key(token), function() FALSE, 1000)
  live$session$wsClosed()
  both_timings(function(before) {
    # The client echoes the update*Input() as an ordinary `update`.
    res <- suppressMessages(resume_with_updates(
      app, list(token = token, inputs = list(txt = "typed")),
      updates = list(list(txt = "from server")), updates_before_flush = before
    ))
    expect_identical(isolate(res$session$input$txt), "from server", info = paste("before =", before))
    msgs <- sent_of(res$ws, "inputMessages")
    expect_identical(msgs[[1]]$inputMessages[[1]]$message$value, "from server")
  })
})

rendered_value <- function(ws, output) {
  html <- sent_of(ws, "values")[[1]]$values[[output]]$html
  sub('.*value="([^"]*)".*', "\\1", html)
}

test_that("a renderUI() input edited while disconnected is rebuilt with the edit, on both paths and timings", {
  local_snapshot_store_dir()
  app <- function(input, output, session) {
    output$form <- renderUI(textInput("name", "Name"))
    output$echo <- renderText(input$name)
  }
  visible <- list(.clientdata_output_form_hidden = FALSE, .clientdata_output_echo_hidden = FALSE)
  both_timings(function(before) {
    res <- suppressMessages(resume_with_updates(app,
      list(inputs = c(list(name = "hello"), visible), current = c(list(name = "hello world"), visible)),
      updates = list(list(name = "hello world")), updates_before_flush = before))
    expect_identical(rendered_value(res$ws, "form"), "hello world", info = paste("inputs-only, before =", before))
    expect_identical(isolate(res$session$input$name), "hello world")
  })
  both_timings(function(before) {
    live <- live_session(app, outputs = c("form", "echo"), inputs = list(name = "hello"))
    token <- live_close(live)
    res <- suppressMessages(resume_with_updates(app,
      list(token = token, inputs = c(list(name = "hello"), visible), current = c(list(name = "hello world"), visible)),
      updates = list(list(name = "hello world")), updates_before_flush = before))
    expect_identical(res$outcome, "snapshot")
    expect_identical(rendered_value(res$ws, "form"), "hello world", info = paste("snapshot, before =", before))
    expect_identical(isolate(res$session$input$name), "hello world")
  })
})

test_that("without `current` (an older client) the restore context comes from `inputs`", {
  local_snapshot_store_dir()
  app <- function(input, output, session) output$form <- renderUI(textInput("name", "Name"))
  res <- suppressMessages(resume_with_updates(app,
    list(inputs = list(name = "hello", .clientdata_output_form_hidden = FALSE))))
  expect_identical(rendered_value(res$ws, "form"), "hello")
})

test_that("with an async output in the first flush, the hold-back clears and a queued click fires once after it resolves", {
  local_snapshot_store_dir()
  spy <- new_spy("obs")
  app <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
    output$n <- renderText(count())
    output$slow <- renderText({
      count()
      promises::promise(function(resolve, reject) later::later(function() resolve("done"), 0.3))
    })
  }
  visible <- list(.clientdata_output_n_hidden = FALSE, .clientdata_output_slow_hidden = FALSE)
  live <- live_session(app, outputs = c("n", "slow"), inputs = list(`plus:shiny.action` = 0))
  for (i in 1:3) live_set(live$session, `plus:shiny.action` = i)
  Sys.sleep(0.4); later::run_now(); live_flush(live$session)
  token <- live_close(live)
  reset_spies()

  ws <- fake_ws()
  s <- ShinySession$new(ws); local_cancel_writes(s)
  run <- function() { s$.serverStarted(); withReactiveDomain(s, isolate(app(input = s$input, output = s$output, session = s))) }
  suppressMessages(s$.resumeSession(list(token = token, inputs = c(list(`plus:shiny.action` = 3L), visible)), run))
  s$manageInputs(withReactiveDomain(s, applyInputHandlers(list(`plus:shiny.action` = 4L), s)))
  flushReact(); s$flushOutput()
  expect_true(isTRUE(s$.resumeHoldBack))          # the slow output keeps the flush open
  expect_equal(spy$obs, 0)
  for (i in 1:30) {
    later::run_now(0.05); flushReact(); s$flushOutput()
    if (!isTRUE(s$.resumeHoldBack)) break
  }
  expect_false(isTRUE(s$.resumeHoldBack))
  tb <- ws$sent()[[1]]$config$resumeToken
  rec <- snapshot_store()$read(snapshot_record_key(tb))
  expect_false(is.null(rec))
  expect_equal(node_value(rec$sources[["|reactiveVal|count"]]), 3)   # the replacement, before the queued click
  for (i in 1:30) {
    later::run_now(0.05); flushReact(); s$flushOutput()
    if (spy$obs == 1) break
  }
  expect_equal(spy$obs, 1)
  expect_equal(as.numeric(isolate(s$input$plus)), 4)
})
