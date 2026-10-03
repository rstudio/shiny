withr::local_envvar(SHINY_RESUME = "TRUE")

configure_mock_writer <- function(session, store, gate = function() TRUE, debounce = 1000) {
  key <- snapshot_record_key(snapshot_token_new())
  session$.snapshotRegistry$configureWriter(store, key, gate, debounce)
  key
}

test_that("a source change marks the session dirty and a debounced write follows the quiet interval", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  isolate(s$env$v(2)); s$flushReact()
  expect_null(store$read(key))
  s$elapse(999); expect_null(store$read(key))
  s$elapse(1); expect_identical(node_value(store$read(key)$sources[["|reactiveVal|v"]]), 2)
  expect_null(store$read(key)$report)
})

test_that("an input change marks the session dirty on its own", {
  s <- snapshot_session(function(input, output, session) NULL)
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  s$setInputs(n = 1)        # no output flushed anything, only the input changed
  s$elapse(1000)
  expect_identical(store$read(key)$inputs$n, 1)
})

test_that("a session that never goes quiet is still written within ten debounce intervals", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(0) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store, debounce = 100)
  for (i in 1:15) { isolate(s$env$v(i)); s$flushReact(); s$elapse(90) }
  expect_false(is.null(store$read(key)))
})

test_that("the writer is gated, pause() stops the closing write, and writeNow() never raises", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  open <- TRUE
  key <- configure_mock_writer(s, store, gate = function() open)
  expect_true(s$.snapshotRegistry$writeNow())
  open <- FALSE
  expect_false(s$.snapshotRegistry$writeNow())
  open <- TRUE
  s$.snapshotRegistry$pause()
  expect_false(s$.snapshotRegistry$writeNow())
  s2 <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  s2$.snapshotRegistry$configureWriter(store, "k", function() stop("broken gate"), 1000)
  expect_message(expect_false(s2$.snapshotRegistry$writeNow()), "broken gate")
})

test_that("a write failure is logged once and the session continues", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  configure_mock_writer(s, store)
  local_mocked_bindings(snapshot_identity = function(session) stop("disk full"))
  expect_message(s$.snapshotRegistry$writeNow(), "Saved state could not be written: disk full")
  expect_silent(s$.snapshotRegistry$writeNow())
})

test_that("a failed write does not stop later debounced writes", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  fail <- TRUE
  identity <- snapshot_identity
  local_mocked_bindings(snapshot_identity = function(session) if (fail) stop("disk full") else identity(session))
  isolate(s$env$v(2)); s$flushReact()
  expect_message(s$elapse(1000), "disk full")
  fail <- FALSE
  isolate(s$env$v(3)); s$flushReact()
  s$elapse(1000)
  expect_identical(node_value(store$read(key)$sources[["|reactiveVal|v"]]), 3)
})

test_that("deleteSnapshot() removes the record and scheduleDelete() removes it later", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  s$.snapshotRegistry$writeNow()
  expect_true(s$.snapshotRegistry$wroteSnapshot())
  s$.snapshotRegistry$deleteSnapshot()
  expect_null(store$read(key))
  s$.snapshotRegistry$writeNow()
  s$.snapshotRegistry$scheduleDelete(0.05)
  Sys.sleep(0.1); later::run_now()
  expect_null(store$read(key))
})
