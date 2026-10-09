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
  dir <- local_app_dir()
  s <- MockShinySession$new()
  id <- snapshot_identity(s)
  expect_identical(id$formatVersion, 3L)
  expect_identical(id$appVersion, "v1")
  expect_null(id$codeHash)                              # a declared version replaces the hash
  expect_identical(id$appPath, normalizePath(dir))
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

test_that("the identity block has no UI fingerprint, so a changed UI never blocks a resume", {
  s <- MockShinySession$new()
  expect_named(snapshot_identity(s), c("appPath", "formatVersion", "appVersion", "codeHash", "user", "groups"))
  expect_null(shinyApp(fluidPage(), function(input, output) NULL)$uiHash)
})

test_that("the code hash covers names and contents of R files in the app dir and R/, nothing else", {
  dir <- local_app_dir()
  expect_null(snapshot_code_hash())                     # an app built in the console
  writeLines("x <- 1", file.path(dir, "app.R"))
  dir.create(file.path(dir, "R"))
  writeLines("f <- function() 1", file.path(dir, "R", "f.R"))
  writeLines("a,b", file.path(dir, "data.csv"))
  dir.create(file.path(dir, "www"))
  writeLines("body {}", file.path(dir, "www", "app.css"))
  h <- snapshot_code_hash()
  expect_match(h, "^[0-9a-f]{32}$")
  writeLines("a,b,c", file.path(dir, "data.csv"))
  writeLines("body { color: red }", file.path(dir, "www", "app.css"))
  expect_identical(snapshot_code_hash(), h)             # non-R files do not count
  writeLines("f <- function() 2", file.path(dir, "R", "f.R"))
  h2 <- snapshot_code_hash()
  expect_false(identical(h2, h))
  file.rename(file.path(dir, "R", "f.R"), file.path(dir, "R", "g.R"))
  expect_false(identical(snapshot_code_hash(), h2))     # a rename counts
})

test_that("without appVersion the identity carries the code hash, computed once per session", {
  dir <- local_app_dir()
  writeLines("x <- 1", file.path(dir, "app.R"))
  s <- MockShinySession$new()
  id <- snapshot_identity(s)
  expect_identical(id$codeHash, snapshot_code_hash())
  writeLines("x <- 2", file.path(dir, "app.R"))
  expect_identical(snapshot_identity(s)$codeHash, id$codeHash)
  s2 <- MockShinySession$new()
  expect_identical(snapshot_identity_mismatch(list(identity = id), s2), "app code changed")
})

test_that("an app with no R files keeps matching: codeHash is NULL on both sides", {
  local_app_dir()
  s <- MockShinySession$new()
  id <- snapshot_identity(s)
  expect_null(id$codeHash)
  expect_null(snapshot_identity_mismatch(list(identity = id), MockShinySession$new()))
})

test_that("resume-off sessions never hash the app's code", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_mocked_bindings(snapshot_code_hash = function(...) stop("hashed"))
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  s$flushReact()
  expect_null(s$.snapshotRegistry)
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

test_that("a claimed key is deleted and refuses later writes until the TTL passes", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  store$write("k", list(a = 1))
  store$write("other", list(b = 2))
  store$claim("k")
  expect_true(store$claimed("k"))
  expect_false(store$claimed("other"))
  expect_null(store$read("k"))
  expect_false(store$write("k", list(a = 2)))
  expect_null(store$read("k"))
  expect_true(store$write("other", list(b = 3)))       # other keys unaffected
  store$.__enclos_env__$private$consumed$set("k", 0)    # remembered long ago
  store$sweep()
  expect_true(store$write("k", list(a = 3)))
})

test_that("app_r_files() lists what loadSupport() loads, in C-locale order", {
  dir <- local_app_dir()
  dir.create(file.path(dir, "R"))
  writeLines("", file.path(dir, "R", "c.R")); writeLines("", file.path(dir, "R", "B.R")); writeLines("", file.path(dir, "R", "a.r"))
  writeLines("", file.path(dir, "app.R")); writeLines("", file.path(dir, "global.R")); writeLines("", file.path(dir, "helpers.R"))
  dir.create(file.path(dir, "R", "sub.R"))
  f <- app_r_files(dir)
  expect_identical(f$helpers, file.path("R", c("B.R", "a.r", "c.R")))
  expect_identical(f$top, c("app.R", "global.R", "helpers.R"))
})

test_that("the code hash includes the server function's package version when given", {
  dir <- local_app_dir()
  writeLines("x <- 1", file.path(dir, "app.R"))
  plain <- snapshot_code_hash(dir)
  withPkg <- snapshot_code_hash(dir, package = "shiny")
  expect_false(identical(plain, withPkg))
  expect_identical(snapshot_code_hash(dir, package = "shiny"), withPkg)
  local_app_dir()
  expect_null(snapshot_code_hash(package = NULL))
  expect_match(snapshot_code_hash(package = "shiny"), "^[0-9a-f]{32}$")
})

test_that("a session notes the server function's package and hashes with it", {
  local_snapshot_store_dir()
  dir <- local_app_dir()
  writeLines("x <- 1", file.path(dir, "app.R"))
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  s$.noteServerFunction(shiny::runApp)
  expect_identical(s$.snapshotRegistry$codeHash(), snapshot_code_hash(package = "shiny"))
})

test_that("devmode names a server function whose file the hash does not cover", {
  # in_devmode() is FALSE under testthat; the resume tests mock the messenger.
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  local_snapshot_store_dir()
  dir <- local_app_dir()
  writeLines("x <- 1", file.path(dir, "app.R"))
  outside <- withr::local_tempfile(fileext = ".R")
  writeLines("server <- function(input, output, session) NULL", outside)
  env <- new.env(parent = globalenv()); source(outside, local = env, keep.source = TRUE)
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  s$.noteServerFunction(env$server)
  expect_length(seen, 1)
  expect_match(seen, "appVersion")
  noSrcref <- eval(parse(text = "function(input, output, session) NULL", keep.source = FALSE), envir = globalenv())
  s2 <- ShinySession$new(fake_ws()); local_cancel_writes(s2)
  s2$.noteServerFunction(noSrcref)                     # app.R is covered; no srcref, no package
  expect_length(seen, 1)
  local_app_dir()                                      # nothing to hash at all
  s3 <- ShinySession$new(fake_ws()); local_cancel_writes(s3)
  s3$.noteServerFunction(noSrcref)
  expect_length(seen, 2)
})

test_that("a declared appVersion silences the devmode code-hash hint", {
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  local_snapshot_store_dir()
  local_enable_resume(appVersion = "v1")
  dir <- local_app_dir()
  writeLines("x <- 1", file.path(dir, "app.R"))
  outside <- withr::local_tempfile(fileext = ".R")
  writeLines("server <- function(input, output, session) NULL", outside)
  env <- new.env(parent = globalenv()); source(outside, local = env, keep.source = TRUE)
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  s$.noteServerFunction(env$server)
  expect_length(seen, 0)
})
