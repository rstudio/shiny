# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("the store writes owner-only files, reads them back, and reports missing keys as NULL", {
  dir <- file.path(withr::local_tempdir(), "snapshots")
  store <- SnapshotStore$new(dir)
  key <- snapshot_record_key(snapshot_token_new())
  store$write(key, list(a = 1))
  expect_identical(store$read(key), list(a = 1))
  expect_null(store$read(snapshot_record_key(snapshot_token_new())))
})

test_that("the store directory and files are owner-only", {
  skip_on_os("windows")
  dir <- file.path(withr::local_tempdir(), "snapshots")
  store <- SnapshotStore$new(dir)
  key <- snapshot_record_key(snapshot_token_new())
  store$write(key, 1)
  expect_identical(file_mode(dir), "700")
  expect_identical(file_mode(file.path(dir, paste0(key, ".rds"))), "600")
})

test_that("a failing UI fingerprint is computed once and never raises", {
  renders <- 0
  local_mocked_bindings(snapshot_ui_fingerprint = function(ui) {
    renders <<- renders + 1
    stop("boom")
  })
  app <- shinyApp(fluidPage(), function(input, output) NULL)
  expect_null(app$uiHash())
  expect_null(app$uiHash())
  expect_identical(renders, 1)
})

test_that("a directory app whose app.R breaks mid-edit yields no UI hash instead of an error", {
  dir <- withr::local_tempdir()
  withr::defer(shinyOptions(appDir = NULL))
  appR <- file.path(dir, "app.R")
  writeLines(c("library(shiny)", "shinyApp(fluidPage('x'), function(input, output) NULL)"), appR)
  app <- as.shiny.appobj(dir)
  expect_match(app$uiHash(), "^[0-9a-f]{32}$")

  Sys.sleep(1.1)
  writeLines("stop('half-edited app')", appR)
  initCurrentAppState(app); withr::defer(clearCurrentAppState())
  expect_no_error(h <- snapshot_ui_hash())
  expect_null(h)
  expect_null(snapshot_identity(MockShinySession$new())$uiHash)
})

test_that("tokens are 128-bit hex, validated before hashing, and keys are hashes", {
  tok <- snapshot_token_new()
  expect_match(tok, "^[0-9a-f]{32}$")
  expect_true(snapshot_token_valid(tok))
  expect_false(snapshot_token_valid("../etc/passwd"))
  expect_false(snapshot_token_valid(c(tok, tok)))
  expect_false(snapshot_token_valid(NULL))
  expect_false(identical(snapshot_record_key(tok), tok))
  expect_match(snapshot_record_key(tok), "^[0-9a-f]{32}$")
})

test_that("the process store lives under R_USER_CACHE_DIR unless a platform save.interface exists", {
  dir <- local_snapshot_store_dir()
  store <- snapshot_store()
  expect_identical(normalizePath(store$directory()), normalizePath(file.path(dir, "R", "shiny", "resume"), mustWork = FALSE))
  expect_identical(snapshot_store(), store)

  platform_dir <- file.path(dir, "platform")
  shinyOptions(save.interface = function(id, callback) {
    d <- file.path(platform_dir, id); dir.create(d, recursive = TRUE, showWarnings = FALSE); callback(d)
  })
  withr::defer(shinyOptions(save.interface = NULL))
  snapshot_store_reset()
  expect_identical(normalizePath(snapshot_store()$directory()), normalizePath(file.path(platform_dir, "snapshots")))
})

test_that("an unwritable store directory disables the feature for the process with one message", {
  local_snapshot_store_dir()
  blocker <- tools::R_user_dir("shiny", "cache")
  dir.create(dirname(blocker), recursive = TRUE, showWarnings = FALSE)
  writeLines("not a directory", blocker)
  expect_message(expect_null(snapshot_store()), "Resume disabled for this process")
  expect_silent(expect_null(snapshot_store()))
})

test_that("a platform save.interface is asked for the store directory once per process", {
  dir <- local_snapshot_store_dir()
  calls <- 0
  shinyOptions(save.interface = function(id, callback) {
    calls <<- calls + 1
    d <- file.path(dir, "platform", id); dir.create(d, recursive = TRUE, showWarnings = FALSE); callback(d)
  })
  withr::defer(shinyOptions(save.interface = NULL))
  store <- snapshot_store()
  expect_identical(snapshot_store(), store)
  expect_equal(calls, 1)
})

test_that("a save.interface that fails is asked once per process, until the store is reset", {
  local_snapshot_store_dir()
  calls <- 0
  shinyOptions(save.interface = function(id, callback) {
    calls <<- calls + 1
    stop("platform unavailable")
  })
  withr::defer(shinyOptions(save.interface = NULL))
  expect_message(expect_null(snapshot_store()), "platform unavailable")
  expect_silent(expect_null(snapshot_store()))
  expect_equal(calls, 1)
  snapshot_store_reset()
  expect_message(expect_null(snapshot_store()), "platform unavailable")
  expect_equal(calls, 2)
})

test_that("an empty directory from a save.interface or the resolver never raises", {
  dir <- local_snapshot_store_dir()
  shinyOptions(save.interface = function(id, callback) callback(""))
  withr::defer(shinyOptions(save.interface = NULL))
  expect_message(store <- expect_no_error(snapshot_store()), "save.interface gave no usable directory")
  expect_identical(normalizePath(store$directory()), normalizePath(file.path(dir, "R", "shiny", "resume")))
  expect_silent(snapshot_store())

  snapshot_store_reset()
  local_mocked_bindings(snapshot_dir = function() "")
  expect_message(expect_null(snapshot_store()), "no usable snapshot directory")
  expect_silent(expect_null(snapshot_store()))
})

test_that("identity block records path, format, declared version and user; mismatches are named", {
  local_enable_resume(appVersion = "v1")
  s <- MockShinySession$new()
  id <- snapshot_identity(s)
  expect_identical(id$formatVersion, 2L)
  expect_identical(id$appVersion, "v1")
  expect_identical(id$appPath, normalizePath(getwd()))
  snap <- list(identity = id)
  expect_null(snapshot_identity_mismatch(snap, s))
  snapv <- snap; snapv$identity$appVersion <- "v2"
  expect_identical(snapshot_identity_mismatch(snapv, s), "appVersion differs")
  snap2 <- snap; snap2$identity$appPath <- "/elsewhere"
  expect_identical(snapshot_identity_mismatch(snap2, s), "app path differs")
  snap3 <- snap; snap3$identity$formatVersion <- 99L
  expect_identical(snapshot_identity_mismatch(snap3, s), "snapshot format version differs")
  s$user <- "bob"
  expect_identical(snapshot_identity_mismatch(snap, s, enforceUser = TRUE), "user differs")
  expect_null(snapshot_identity_mismatch(snap, s, enforceUser = FALSE))
  expect_identical(snapshot_identity_mismatch(list(), s), "unknown snapshot format")
})

test_that("the UI fingerprint follows the rendered page for static UIs and the body for function UIs", {
  with_app <- function(app, expr) {
    initCurrentAppState(app); withr::defer(clearCurrentAppState())
    force(expr)
  }
  srv <- function(input, output, session) NULL
  a1 <- with_app(shinyApp(fluidPage(textInput("a", "a")), srv), snapshot_ui_hash())
  a2 <- with_app(shinyApp(fluidPage(textInput("a", "a")), srv), snapshot_ui_hash())
  b  <- with_app(shinyApp(fluidPage(textInput("b", "b")), srv), snapshot_ui_hash())
  expect_match(a1, "^[0-9a-f]{32}$")
  expect_identical(a1, a2)
  expect_false(identical(a1, b))
  f1 <- with_app(shinyApp(function(req) fluidPage("x"), srv), snapshot_ui_hash())
  f2 <- with_app(shinyApp(function(req) fluidPage("y"), srv), snapshot_ui_hash())
  expect_false(identical(f1, f2))
  # A lazy fingerprint (ui.R apps) is evaluated at call time.
  app <- shinyApp(fluidPage(), srv); app$uiHash <- function() "abc"
  expect_identical(with_app(app, snapshot_ui_hash()), "abc")
  expect_null(snapshot_ui_hash())   # no app state
  s <- MockShinySession$new()
  expect_identical(with_app(shinyApp(fluidPage(), srv), snapshot_identity(s)$uiHash), with_app(shinyApp(fluidPage(), srv), snapshot_ui_hash()))
})

test_that("shinyApp() never raises because its UI cannot be fingerprinted", {
  unrenderable <- structure(
    list(name = "div", attribs = list(), children = list(function() 1)),
    class = "shiny.tag"
  )
  app <- shinyApp(unrenderable, function(input, output) NULL)
  expect_null(app$uiHash())
})

test_that("directory apps expose a lazy UI fingerprint", {
  dir <- withr::local_tempdir()
  withr::defer(shinyOptions(appDir = NULL))
  writeLines(c(
    "library(shiny)",
    "shinyApp(fluidPage(textInput('a', 'a')), function(input, output) NULL)"
  ), file.path(dir, "app.R"))
  app <- as.shiny.appobj(dir)
  expect_true(is.function(app$uiHash))
  expect_match(app$uiHash(), "^[0-9a-f]{32}$")

  dir2 <- withr::local_tempdir()
  withr::defer(shinyOptions(appDir = NULL))
  writeLines("fluidPage('x')", file.path(dir2, "ui.R"))
  writeLines("function(input, output) NULL", file.path(dir2, "server.R"))
  app2 <- as.shiny.appobj(dir2)
  h1 <- app2$uiHash()
  expect_match(h1, "^[0-9a-f]{32}$")
  writeLines("fluidPage('y')", file.path(dir2, "ui.R"))
  expect_false(identical(app2$uiHash(), h1))
})

test_that("the store applies a restrictive umask while writing", {
  skip_on_os("windows")
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  old <- Sys.umask("022")
  withr::defer(Sys.umask(old))
  store$write("k", 1)
  expect_identical(as.character(Sys.umask(NA)), "22")
})


test_that("the record key includes the app path, so another app's record is never read", {
  tok <- snapshot_token_new()
  expect_false(identical(snapshot_record_key(tok, "/apps/a"), snapshot_record_key(tok, "/apps/b")))
  expect_identical(snapshot_record_key(tok, "/apps/a"), snapshot_record_key(tok, "/apps/a"))
})

test_that("the store caps total size and record count with fifo eviction", {
  dir <- file.path(withr::local_tempdir(), "snapshots")
  store <- SnapshotStore$new(dir, max_n = 2)
  for (k in c("k1", "k2", "k3")) { store$write(k, k); Sys.sleep(0.05) }
  store$sweep()   # cachem prunes lazily; sweep() forces it
  expect_null(store$read("k1"))
  expect_identical(store$read("k3"), "k3")
  small <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"), max_size = 400)
  # Incompressible bytes: cachem measures the compressed file size.
  noise <- function() as.raw(sample(0:255, 300, replace = TRUE))
  small$write("a", noise()); Sys.sleep(0.05); small$write("b", noise())
  small$sweep()
  expect_null(small$read("a"))
  expect_identical(length(small$read("b")), 300L)
})
