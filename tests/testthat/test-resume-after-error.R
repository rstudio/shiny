withr::local_envvar(SHINY_RESUME = "TRUE")

error_app <- function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$boom, stop("kaboom"))
  output$n <- renderText(count())
}

crash <- function(live) {
  # The error is reported through warning(immediate. = TRUE) by printError().
  suppressWarnings(live_set(live$session, `boom:shiny.action` = 1))
  live
}

test_that("a fatal error pauses the writer, sends fatalError with the message and saved, then ends the session", {
  local_snapshot_store_dir()
  live <- live_session(error_app, outputs = "n", inputs = list(`plus:shiny.action` = 0))
  live_set(live$session, `plus:shiny.action` = 1)
  live$session$.snapshotRegistry$writeNow()
  key <- live$session$.snapshotRegistry$storeKey()
  crash(live)
  fatal <- sent_of(live$ws, "fatalError")
  expect_length(fatal, 1)
  expect_identical(fatal[[1]]$fatalError$message, "kaboom")
  expect_true(fatal[[1]]$fatalError$saved)
  msgs <- live$ws$sent()
  iFatal <- which(vapply(msgs, function(m) !is.null(m$fatalError), logical(1)))
  iAllow <- which(vapply(msgs, function(m) !is.null(m$allowReconnect), logical(1)))
  expect_true(iFatal < iAllow)
  expect_identical(msgs[[iAllow]]$allowReconnect, FALSE)
  expect_true(live$ws$closed)
  expect_true(live$session$.snapshotRegistry$paused())
  # A second fatal error before the socket closes sends nothing more.
  suppressWarnings(live$session$unhandledError(simpleError("again"), close = TRUE))
  expect_length(sent_of(live$ws, "fatalError"), 1)
  expect_length(sent_of(live$ws, "allowReconnect"), 1)
  # The record from before the failing flush survives, and nothing more is written.
  live_set(live$session, `plus:shiny.action` = 2)
  live$session$wsClosed()
  rec <- snapshot_store()$read(key)
  expect_false(is.null(rec))
  expect_identical(node_value(rec$sources[["|reactiveVal|count"]]), 1)
})

test_that("before any write, saved is FALSE", {
  local_snapshot_store_dir()
  live <- live_session(error_app, outputs = "n")
  crash(live)
  expect_identical(sent_of(live$ws, "fatalError")[[1]]$fatalError$saved, FALSE)
})

test_that("the dialog's error text follows shiny.sanitize.errors", {
  local_snapshot_store_dir()
  withr::local_options(shiny.sanitize.errors = TRUE)
  live <- live_session(error_app, outputs = "n")
  crash(live)
  fatal <- sent_of(live$ws, "fatalError")[[1]]$fatalError
  expect_null(fatal$message)
  expect_identical(fatal$saved, FALSE)
})

test_that("with resume off a fatal error closes the socket as on main, with no fatalError message", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  live <- live_session(error_app, outputs = "n")
  crash(live)
  expect_true(live$ws$closed)
  expect_length(sent_of(live$ws, "fatalError"), 0)
  expect_length(sent_of(live$ws, "allowReconnect"), 0)
})

test_that("an onUnhandledError() callback that closes the session wins: no dialog is offered", {
  local_snapshot_store_dir()
  live <- live_session(function(input, output, session) {
    session$onUnhandledError(function(e) session$close())
    observeEvent(input$boom, stop("kaboom"))
  })
  crash(live)
  expect_true(live$ws$closed)
  expect_length(sent_of(live$ws, "allowReconnect"), 1)
  expect_length(sent_of(live$ws, "fatalError"), 0)
})

test_that("an async observer's rejection takes the same path", {
  local_snapshot_store_dir()
  live <- live_session(function(input, output, session) {
    # An error raised in a promise callback, as a real async observer fails.
    # (promise_reject(simpleError()) carries no call stack for printError().)
    observeEvent(input$boom, promises::then(promises::promise_resolve(1), function(x) stop("async kaboom")))
  })
  suppressWarnings({
    live_set(live$session, `boom:shiny.action` = 1)
    drain_later()
  })
  fatal <- sent_of(live$ws, "fatalError")
  expect_length(fatal, 1)
  expect_identical(fatal[[1]]$fatalError$message, "async kaboom")
})

test_that("testServer() keeps ending the session on a fatal error", {
  suppressWarnings(testServer(error_app, {
    session$setInputs(boom = 1)
    expect_true(session$isClosed())
  }))
})

test_that("the Fatal error OTel event is unchanged", {
  local_snapshot_store_dir()
  logged <- list()
  local_mocked_bindings(
    has_otel_collect = function(collect) TRUE,
    otel_log = function(msg, ..., attributes = NULL, severity = "info", logger = NULL) {
      logged[[length(logged) + 1L]] <<- list(msg = msg, severity = severity, attributes = attributes)
    }
  )
  live <- live_session(error_app, outputs = "n")
  crash(live)
  fatal <- Filter(function(l) identical(l$msg, "Fatal error"), logged)
  expect_length(fatal, 1)
  expect_identical(fatal[[1]]$severity, "fatal")
  expect_identical(fatal[[1]]$attributes$session.id, live$session$token)
  expect_setequal(names(fatal[[1]]$attributes), c("session.id", "error"))
})
