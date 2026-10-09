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

test_that("an input change still marks the session dirty (uploads arrive as inputs), and the record holds no input values", {
  s <- snapshot_session(function(input, output, session) NULL)
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  s$setInputs(n = 1)        # no output flushed anything, only the input changed
  s$elapse(1000)
  rec <- store$read(key)
  expect_false(is.null(rec))
  expect_null(rec$inputs)
})

test_that("a session that never goes quiet is still written within ten debounce intervals", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(0) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store, debounce = 100)
  for (i in 1:15) { isolate(s$env$v(i)); s$flushReact(); s$elapse(90) }
  expect_false(is.null(store$read(key)))
})

test_that("the writer is gated and writeNow() never raises", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  open <- TRUE
  key <- configure_mock_writer(s, store, gate = function() open)
  expect_true(s$.snapshotRegistry$writeNow())
  open <- FALSE
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

test_that("deleteSnapshot() removes the record and cancels a pending write", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  s$.snapshotRegistry$writeNow()
  expect_true(s$.snapshotRegistry$wroteSnapshot())
  isolate(s$env$v(2)); s$flushReact()                 # schedules a debounced write
  s$.snapshotRegistry$deleteSnapshot()
  expect_null(store$read(key))
  s$elapse(1000)
  expect_null(store$read(key))                        # the pending write was cancelled
  expect_false("scheduleDelete" %in% names(SnapshotRegistry$public_methods))
  expect_false("pause" %in% names(SnapshotRegistry$public_methods))
})

test_that("writer snapshots omit reports without changing saved state", {
  s <- snapshot_session(function(input, output, session) {
    v <- reactiveVal(1)
    rv <- reactiveValues(a = 2)
  })
  full <- s$.snapshotRegistry$snapshot()
  full$report <- NULL
  expect_identical(s$.snapshotRegistry$snapshot(report = FALSE), full)

  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  key <- configure_mock_writer(s, store)
  local_mocked_bindings(snapshot_report_frame = function(...) stop("report constructed"))
  expect_identical(s$.snapshotRegistry$writeNow(), TRUE)
  expect_identical(node_value(store$read(key)$sources[["|reactiveVal|v"]]), 1)
})

test_that("an immediate write clears changes awaiting a flush", {
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  writes <- 0L
  store <- list(write = function(key, value) writes <<- writes + 1L,
                claimed = function(key) FALSE, filesDir = function(key) file.path(tempdir(), "none"))
  configure_mock_writer(s, store)
  isolate(s$env$v(2))
  expect_identical(s$.snapshotRegistry$writeNow(), TRUE)
  s$flushReact()
  s$elapse(1000)
  expect_identical(writes, 1L)
})

test_that("a failed write keeps the previous record's upload copies", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "a.csv", "first")
  reg <- live$session$.snapshotRegistry
  expect_true(reg$writeNow())
  key <- live_key(live); store <- snapshot_store()
  old <- list.files(store$filesDir(key))
  live_upload(live$session, "f", "b.csv", "second")
  reg$configureWriter(list(write = function(key, value) stop("disk full"), filesDir = store$filesDir,
                           claimed = function(key) FALSE,
                           delete = function(key) invisible()),
                      key, function() TRUE, 1000)
  expect_message(expect_false(reg$writeNow()), "could not be written: disk full")
  expect_true(all(file.exists(file.path(store$filesDir(key), old))))
})

test_that("a write to a claimed key copies no uploads", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "a.csv", "first")
  reg <- live$session$.snapshotRegistry
  expect_true(reg$writeNow())
  key <- live_key(live); store <- snapshot_store()
  store$claim(key)
  expect_false(reg$writeNow())
  expect_false(dir.exists(store$filesDir(key)))
})

test_that("a write after the last upload is gone removes its copy", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "a.csv", "first")
  reg <- live$session$.snapshotRegistry
  expect_true(reg$writeNow())
  key <- live_key(live); store <- snapshot_store()
  expect_length(list.files(store$filesDir(key)), 1)
  live_set(live$session, f = NULL)
  expect_true(reg$writeNow())
  expect_identical(list.files(store$filesDir(key)), character(0))
})

test_that("SnapshotWriter writes the record its callback builds, once the gate allows", {
  dir <- local_snapshot_store_dir()
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  open <- TRUE
  w <- SnapshotWriter$new(s, function() snapshot_store(), "k1", function() open, 1000)
  builds <- 0
  record <- function() { builds <<- builds + 1; list(complete = TRUE, sources = list(), userData = list(), identity = list()) }
  expect_true(w$writeNow(record))
  expect_equal(builds, 1)
  expect_true(w$wrote())
  open <- FALSE
  expect_false(w$writeNow(record))
  expect_equal(builds, 1)                      # the gate is checked before building
  open <- TRUE
  w$delete()
  expect_false(w$wrote())
  expect_null(snapshot_store()$read("k1"))
})
