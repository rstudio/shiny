# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

fake_ws <- function() {
  sent <- list()
  list(request = list(), send = function(msg) sent[[length(sent) + 1]] <<- msg, close = function() NULL,
       sent = function() lapply(sent, jsonlite::fromJSON, simplifyVector = FALSE))
}

configure_mock_writer <- function(session, store, gate = function() TRUE, debounce = 1000) {
  key <- snapshot_token_key(snapshot_token_new())
  session$.snapshotRegistry$configureWriter(store, key, gate, debounce)
  key
}

test_that("a source change marks the session dirty and a debounced write follows the quiet interval", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  server <- function(input, output, session) { v <- reactiveVal(0); observe({ input$x }) }
  s <- snapshot_session(server)
  key <- configure_mock_writer(s, store)
  s$flushReact()
  expect_null(store$read(key))                     # nothing changed yet
  isolate(s$env$v(1)); s$flushReact()
  s$elapse(500)
  expect_null(store$read(key))                     # still inside the quiet interval
  isolate(s$env$v(2)); s$flushReact()
  s$elapse(500)
  expect_null(store$read(key))                     # the timer was reset
  s$elapse(600)
  snap <- store$read(key)
  expect_identical(node_value(snap$nodes[[grep("reactiveVal:v#1$", names(snap$nodes))]]), 2)
  expect_true(s$.snapshotRegistry$wroteSnapshot())
  s$setInputs(x = 1)                                # input changes count too
  s$elapse(1100)
  expect_identical(store$read(key)$inputs$x, 1)
})

test_that("a session that never goes quiet is still written within ten debounce intervals", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  server <- function(input, output, session) { v <- reactiveVal(0) }
  s <- snapshot_session(server)
  key <- configure_mock_writer(s, store)
  written_at <- NA
  for (i in 1:30) {                 # a change every 500 ms; the debounce alone would never settle
    isolate(s$env$v(i)); s$flushReact(); s$elapse(500)
    if (!is.null(store$read(key))) { written_at <- i; break }
  }
  expect_false(is.na(written_at))
  expect_lte(written_at, 21)         # by about ten seconds
  expect_gte(written_at, 19)         # and not before the cap
})

test_that("the writer is gated and writeNow() forces a write when allowed", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  allowed <- TRUE
  server <- function(input, output, session) { v <- reactiveVal(0) }
  s <- snapshot_session(server)
  key <- configure_mock_writer(s, store, gate = function() allowed)
  isolate(s$env$v(1)); s$flushReact(); s$elapse(1100)
  expect_false(is.null(store$read(key)))
  allowed <- FALSE
  isolate(s$env$v(2)); s$flushReact(); s$elapse(1100)
  expect_identical(node_value(store$read(key)$nodes[[1]]), 1)
  expect_false(s$.snapshotRegistry$writeNow())
  allowed <- TRUE
  expect_true(s$.snapshotRegistry$writeNow())
  expect_identical(node_value(store$read(key)$nodes[[1]]), 2)
})

test_that("the snapshot written carries the identity block and is readable with enforceUser", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  key <- configure_mock_writer(s, store)
  s$.snapshotRegistry$writeNow()
  snap <- store$read(key)
  expect_null(snapshot_identity_mismatch(snap, s, enforceUser = TRUE))
  expect_identical(snap$identity$formatVersion, 1L)
})

test_that("two sessions in one process write under their own keys and dedupe devmode messages separately", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  server <- function(input, output, session) { v <- reactiveVal(1) }
  s1 <- snapshot_session(server); s2 <- snapshot_session(server)
  k1 <- configure_mock_writer(s1, store); k2 <- configure_mock_writer(s2, store)
  isolate(s2$env$v(2))
  s1$.snapshotRegistry$writeNow(); s2$.snapshotRegistry$writeNow()
  expect_identical(node_value(store$read(k1)$nodes[[1]]), 1)
  expect_identical(node_value(store$read(k2)$nodes[[1]]), 2)
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  s1$.snapshotRegistry$devmodeInform("k", "hello"); s1$.snapshotRegistry$devmodeInform("k", "hello")
  s2$.snapshotRegistry$devmodeInform("k", "hello")
  expect_length(seen, 2)
})

test_that("a write failure is logged once per session and the session continues", {
  store <- list(write = function(key, value) stop("disk full"))  # the registry only calls write()
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
  configure_mock_writer(s, store)
  expect_message(expect_false(s$.snapshotRegistry$writeNow()), "disk full")
  expect_silent(expect_false(s$.snapshotRegistry$writeNow()))
})

test_that("a failing write after the first adopt commit does not stop changed live inputs from applying", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observe({ input$a; spy$a <- spy$a + 1 })
  }}
  spy <- new_spy("a")
  second <- MockShinySession$new()
  configure_mock_writer(second, list(write = function(key, value) stop("disk full")))
  expect_message(
    rt <- snapshot_roundtrip(make(spy), function(s) s$setInputs(a = 1), live = list(a = 2), second = second),
    "disk full"
  )
  expect_equal(spy$a, 1)
  expect_identical(isolate(second$input$a), 2)
})

test_that("with resume on, config carries a token and allowReconnect(TRUE) and \"force\" change nothing", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  cfg <- ws$sent()[[1]]$config
  expect_match(cfg$resumeToken, "^[0-9a-f]{32}$")
  expect_false("allowReconnect" %in% names(cfg))   # the token is what makes the client retry
  n <- length(ws$sent())
  expect_silent(s$allowReconnect(TRUE))
  expect_silent(s$allowReconnect("force"))
  expect_length(ws$sent(), n)
  expect_true(s$.snapshotRegistry$writerEnabled())
})

test_that("with resume on, allowReconnect(FALSE) is an error pointing to disableResume()", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  n <- length(ws$sent())
  expect_error(
    s$allowReconnect(FALSE),
    "Reconnecting is part of resume; call disableResume() at the top level of the app to turn both off.",
    fixed = TRUE
  )
  expect_length(ws$sent(), n)
  expect_true(s$.snapshotRegistry$writerEnabled())
  expect_error(s$allowReconnect("yes"), 'value must be TRUE, FALSE, or "force"', fixed = TRUE)
})

test_that("with resume off, allowReconnect() is main's: no token, no registry, values forwarded", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  cfg <- ws$sent()[[1]]$config
  expect_null(cfg$resumeToken)
  expect_false("allowReconnect" %in% names(cfg))   # as on main
  for (value in list(TRUE, "force", FALSE)) {
    s$allowReconnect(value)
    expect_identical(ws$sent()[[length(ws$sent())]]$allowReconnect, value)
  }
  expect_null(s$.snapshotRegistry)
})

test_that("with the feature off a session never resolves the store, so no cache directory is created", {
  ws <- list(request = list(), send = function(msg) NULL, close = function() NULL)
  withr::local_envvar(SHINY_RESUME = "FALSE")
  dir <- local_snapshot_store_dir()
  s <- ShinySession$new(ws)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  s$wsClosed()
  expect_length(list.files(dir, recursive = TRUE, include.dirs = TRUE), 0)
  expect_null(.globals$snapshotStores)
})

test_that("a store directory that cannot be resolved disables writing without raising", {
  ws <- list(request = list(), send = function(msg) NULL, close = function() NULL)
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  local_mocked_bindings(snapshot_dir = function() stop("no save.interface dir"))
  s <- ShinySession$new(ws)
  expect_message(expect_false(s$.snapshotRegistry$writeNow()), "no save.interface dir")
  expect_silent(expect_false(s$.snapshotRegistry$writeNow()))
  s2 <- ShinySession$new(ws)
  expect_silent(expect_false(s2$.snapshotRegistry$writeNow()))   # one message per process
})

test_that(".endForGood() deletes the saved state, stops writing, and tells the client once", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  key <- snapshot_token_key(s$.__enclos_env__$private$resumeToken)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  expect_true(s$.snapshotRegistry$writeNow())
  s$.endForGood()
  s$.endForGood()
  expect_null(snapshot_store()$read(key))
  expect_false(s$.snapshotRegistry$writeNow())
  s$wsClosed()
  expect_null(snapshot_store()$read(key))
  falses <- Filter(function(m) identical(m$allowReconnect, FALSE), ws$sent())
  expect_length(falses, 1)
})

test_that(".endForGood() still tells the client when deleting the saved state fails", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_mocked_bindings(snapshot_store = function() {
    list(write = function(key, value) invisible(), delete = function(key) stop("disk gone"))
  })
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  expect_true(s$.snapshotRegistry$writeNow())
  expect_message(s$.endForGood(), "Saved state could not be deleted: disk gone")
  expect_identical(ws$sent()[[length(ws$sent())]]$allowReconnect, FALSE)
})

test_that("a gated-off writeNow() still cancels the pending debounced write", {
  store <- SnapshotStore$new(file.path(withr::local_tempdir(), "s"))
  allowed <- TRUE
  s <- snapshot_session(function(input, output, session) { v <- reactiveVal(0) })
  configure_mock_writer(s, store, gate = function() allowed)
  isolate(s$env$v(1)); s$flushReact()
  timer <- s$.__enclos_env__$private$timer
  expect_false(is.infinite(timer$timeToNextEvent()))
  allowed <- FALSE
  expect_false(s$.snapshotRegistry$writeNow())
  expect_true(is.infinite(timer$timeToNextEvent()))
})

test_that("wsClosed() forces a write, and upload directories outlive the session while its snapshot exists", {
  sent <- list()
  ws <- list(request = list(), send = function(msg) sent[[length(sent) + 1]] <<- msg, close = function() NULL)
  local_snapshot_store_dir()
  s <- ShinySession$new(ws)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  fuc <- s$.__enclos_env__$private$fileUploadContext
  id <- fuc$createUploadOperation(list(list(name = "a", size = 1, type = "text/plain")))
  dir <- file.path(tempdir(), id)
  expect_true(dir.exists(dir))
  key <- snapshot_token_key(s$.__enclos_env__$private$resumeToken)
  wr <- rlang::new_weakref(s)
  timers <- NROW(timerCallbacks$.times)
  # The TTL backstop must not sit in the later loop for a day: code that
  # drains it (wait_for_it(), `while (!later::loop_empty())`) would spin.
  later::with_temp_loop({
    s$wsClosed()
    expect_true(later::loop_empty())
  })
  expect_false(is.null(snapshot_store()$read(key)))
  expect_true(dir.exists(dir))                     # retained while the snapshot exists
  # The deferred removal must not pin the session. If this fails, find
  # the retainer before weakening the test. `v` goes too: a source reaches
  # its session through its registry, as any app-held reactive would.
  rm(s, v); gc()
  expect_null(rlang::wref_key(wr))
  snapshot_store()$delete(key)
  expect_false(dir.exists(dir))
  expect_identical(NROW(timerCallbacks$.times), timers)  # the TTL backstop went with it
})

# What runApp() does with an app object before sessions start.
settings_in <- function(app) {
  initCurrentAppState(app)
  on.exit(clearCurrentAppState())
  applyCapturedAppOptions(app$appOptions)
  ShinySession$new(fake_ws())$.resumeSettings
}

test_that("each app's sessions use that app's resume settings", {
  local_snapshot_store_dir()
  withr::local_envvar(SHINY_RESUME = NA, SHINY_PORT = NA)
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  enableResume(maxSize = 1e6)
  a <- shinyApp(fluidPage(), function(input, output, session) NULL)
  disableResume()
  b <- shinyApp(fluidPage(), function(input, output, session) NULL)
  c3 <- shinyApp(fluidPage(), function(input, output, session) NULL)
  expect_identical(settings_in(a)$maxSize, 1e6)
  expect_false(settings_in(b)$enabled)
  expect_identical(settings_in(c3), c(list(enabled = TRUE), resume_defaults()))
})

test_that("a session resolves its settings once and the writer, caps and identity read them", {
  local_snapshot_store_dir()
  local_enable_resume(appVersion = "v3", debounce = 50)
  s <- ShinySession$new(fake_ws())
  expect_identical(s$.resumeSettings$appVersion, "v3")
  expect_identical(snapshot_identity(s)$appVersion, "v3")
  expect_identical(s$.snapshotRegistry$.__enclos_env__$private$debounceMs, 50)
  disableResume()
  expect_true(s$.resumeSettings$enabled)               # resolved at start
  expect_false(ShinySession$new(fake_ws())$.resumeSettings$enabled)
  expect_false(MockShinySession$new()$.resumeSettings$enabled)
})

resume_server <- function(spy) { force(spy); function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
  output$n <- renderText(count())
  session$onResumed(function(info) spy$info <- info)
}}

# Drives a real ShinySession through the resume branch exactly as server.R
# does: data is the decoded `resume` payload. `spy$received` records what
# session$onInputReceived() callbacks see; the callback is registered before
# the resume, since the server function runs after inputs are received.
resume_real <- function(server, data, ws = fake_ws(), spy = NULL, before = function(s) NULL) {
  s <- ShinySession$new(ws)
  if (!is.null(spy)) s$onInputReceived(function(data) spy$received <- names(data))
  before(s)
  run <- function() withReactiveDomain(s, isolate(server(input = s$input, output = s$output, session = s)))
  info <- s$.resumeSession(data, run)
  flushReact(); s$flushOutput()
  list(session = s, info = info, ws = ws)
}

# manageInputs() takes handler-applied values; server.R applies the handlers
# before it, so do the same here (values arrive from JSON as doubles).
set_plus <- function(s, i) s$manageInputs(applyInputHandlers(list(`plus:shiny.action` = i), s), now = TRUE)

# A live session that ran resume_server() with output$n visible, clicked
# `plus` three times, and closed (writing its snapshot). Returns its resume
# token.
closed_plus_session <- function(spy) {
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  withReactiveDomain(s, isolate(resume_server(spy)(s$input, s$output, s)))
  s$manageInputs(list(.clientdata_output_n_hidden = FALSE), now = TRUE)
  set_plus(s, 0); flushReact()
  for (i in c(1, 2, 3)) { set_plus(s, i); flushReact() }
  s$wsClosed()
  ws$sent()[[1]]$config$resumeToken
}

test_that("a valid token with a matching snapshot adopts, sends `resumed`, logs, and deletes the snapshot", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  key <- snapshot_token_key(token)
  expect_false(is.null(snapshot_store()$read(key)))

  spy2 <- new_spy("obs")
  expect_message(
    res <- resume_real(resume_server(spy2), list(token = token, dom = "intact", inputs = list(`plus:shiny.action` = 3)), spy = spy2),
    "Resumed \\(reconnect\\): restored"
  )
  expect_identical(res$info, list(from = "reconnect", resumed = "snapshot", dom = "intact"))
  expect_identical(spy2$info, res$info)
  expect_identical(spy2$received, "plus")           # onInputReceived fires on resume
  expect_equal(spy2$obs, 0)
  expect_equal(isolate(res$session$input$plus), structure(3, class = c("shinyActionButtonValue", "numeric")))
  msgs <- res$ws$sent()
  resumed <- Filter(function(m) !is.null(m$resumed), msgs)
  expect_length(resumed, 1)
  expect_identical(resumed[[1]], list(resumed = "snapshot", from = "reconnect", dom = "intact"))
  expect_identical(msgs[[2]], resumed[[1]])         # first message after config
  expect_true(which(vapply(msgs, function(m) !is.null(m$resumed), logical(1))) <
              which(vapply(msgs, function(m) !is.null(m$values), logical(1)))[1])
  expect_null(snapshot_store()$read(key))
  # The new session's snapshot exists as soon as adoption committed.
  newToken <- msgs[[1]]$config$resumeToken
  expect_false(identical(newToken, token))
  expect_false(is.null(snapshot_store()$read(snapshot_token_key(newToken))))
})

test_that("on a real session, changed live inputs apply after the first flush and their outputs reach the client", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))

  spy <- new_spy("obs")
  expect_message(
    res <- resume_real(resume_server(spy), list(token = token, dom = "intact", inputs = list(`plus:shiny.action` = 4, .clientdata_output_n_hidden = FALSE))),
    "Resumed \\(reconnect\\): restored"
  )
  # The first flush adopted everything and its onFlushed applied the
  # changed `plus`.
  expect_equal(spy$obs, 0)
  expect_false(res$session$.snapshotRegistry$inAdoptMode())
  expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
  expect_length(Filter(function(m) !is.null(m$values$n), res$ws$sent()), 0)
  # The service loop's next tick runs the dependents and flushes them.
  flushReact(); res$session$flushOutput()
  expect_equal(spy$obs, 1)
  values <- Filter(function(m) !is.null(m$values$n), res$ws$sent())
  expect_identical(values[[length(values)]]$values$n, "4")
})

test_that("malformed, unknown, and mismatched tokens fall through to inputs-only with the cause logged", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  spy <- new_spy("obs")
  cases <- list(
    list(data = list(token = "../x", dom = "intact", inputs = list()), cause = "malformed token"),
    list(data = list(token = snapshot_token_new(), dom = "intact", inputs = list()), cause = "no snapshot"),
    list(data = list(token = snapshot_token_new(), share = "abc", dom = "intact", inputs = list()), cause = "malformed token"),
    list(data = list(dom = "intact", inputs = list()), cause = "malformed token")
  )
  for (case in cases) {
    expect_message(res <- resume_real(resume_server(spy), case$data), paste0("from inputs only: ", case$cause))
    expect_identical(res$info$resumed, "inputs")
    resumed <- Filter(function(m) !is.null(m$resumed), res$ws$sent())
    expect_identical(resumed[[1]]$resumed, "inputs")
  }
  # identity mismatch
  ws1 <- fake_ws(); s1 <- ShinySession$new(ws1)
  withReactiveDomain(s1, isolate(resume_server(spy)(s1$input, s1$output, s1)))
  token <- ws1$sent()[[1]]$config$resumeToken
  s1$wsClosed()
  local({
    local_enable_resume(appVersion = "v2")
    expect_message(res <- resume_real(resume_server(spy), list(token = token, dom = "intact", inputs = list())), "appVersion differs")
    expect_identical(res$info$resumed, "inputs")
  })
})

test_that("every rejection cause falls through to inputs-only and is logged", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  expect_resume_cause <- function(data, cause) {
    expect_message(res <- resume_real(resume_server(new_spy("obs")), data), cause)
    resumed <- Filter(function(m) !is.null(m$resumed), res$ws$sent())
    expect_identical(resumed[[1]]$resumed, "inputs")
    res
  }
  resume <- function(token) list(token = token, dom = "intact", inputs = list())

  token <- closed_plus_session(new_spy("obs"))
  expect_resume_cause(list(token = token, dom = "fresh", inputs = list()), "from inputs only: dom is not intact")

  local({
    local_mocked_bindings(snapshot_store = function() NULL)
    expect_resume_cause(resume(token), "from inputs only: store unavailable")
  })
  local({
    local_mocked_bindings(snapshot_store = function() list(read = function(key) stop("disk gone")))
    expect_resume_cause(resume(token), "from inputs only: snapshot unreadable: disk gone")
  })

  store <- snapshot_store()
  key <- snapshot_token_key(token)
  snap <- store$read(key)
  snap$identity$user <- "someone else"
  store$write(key, snap)
  expect_resume_cause(resume(token), "from inputs only: user differs")
})

test_that("a malformed stored snapshot falls through to inputs-only instead of raising", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  store <- snapshot_store()
  key <- snapshot_token_key(token)
  good <- store$read(key)
  bad <- list(
    list(value = "garbage", cause = "snapshot unreadable: not a list"),
    list(value = list(nodes = list()), cause = "snapshot unreadable: no identity block"),
    list(value = utils::modifyList(good, list(inputs = "x")), cause = "snapshot unreadable: `inputs` is not a list"),
    list(value = utils::modifyList(good, list(fileInputs = "plus")), cause = "snapshot unreadable: file input `plus` is not a list")
  )
  for (case in bad) {
    store$write(key, case$value)
    spy <- new_spy("obs")
    expect_message(res <- resume_real(resume_server(spy), list(token = token, dom = "intact", inputs = list())), case$cause, fixed = TRUE)
    expect_identical(res$info$resumed, "inputs")
    resumed <- Filter(function(m) !is.null(m$resumed), res$ws$sent())
    expect_identical(resumed[[1]]$resumed, "inputs")
  }
})

test_that("an adoption failure before the server function runs falls back and runs the server function once", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  # .resumeSession() seeds the restore context once before adopt() and adopt()
  # seeds it again at step 1; fail the second, inside adopt().
  seeds <- 0
  local_mocked_bindings(snapshot_seed_restore_context = function(session, inputs) {
    seeds <<- seeds + 1
    if (seeds == 2) stop("boom")
    ctx <- RestoreContext$new()
    ctx$set(active = FALSE, input = as.list(inputs))
    session$restoreContext <- ctx
  })
  runs <- 0
  server <- function(input, output, session) runs <<- runs + 1
  expect_message(
    res <- resume_real(server, list(token = token, dom = "intact", inputs = list(`plus:shiny.action` = 3))),
    "from inputs only: adoption failed: boom"
  )
  expect_identical(res$info$resumed, "inputs")
  expect_equal(runs, 1)
  expect_false(res$session$.snapshotRegistry$inAdoptMode())
  resumed <- Filter(function(m) !is.null(m$resumed), res$ws$sent())
  expect_length(resumed, 1)
  expect_identical(resumed[[1]]$resumed, "inputs")
})

test_that("an error in the server function propagates from a resume, which runs it once and sends `resumed` first", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  runs <- 0
  server <- function(input, output, session) {
    runs <<- runs + 1
    session$sendCustomMessage("hello", list())
    stop("user error")
  }
  ws <- fake_ws()
  expect_error(resume_real(server, list(token = token, dom = "intact", inputs = list()), ws = ws), "user error")
  expect_equal(runs, 1)
  msgs <- ws$sent()
  expect_identical(msgs[[2]]$resumed, "snapshot")
  expect_false(is.null(msgs[[3]]$custom$hello))
})

test_that("a changed UI fingerprint asks the client to reload, keeps the snapshot, and resumes from inputs meanwhile", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  srv <- function(input, output, session) NULL
  initCurrentAppState(shinyApp(fluidPage(textInput("a", "a")), srv)); withr::defer(clearCurrentAppState())
  spy <- new_spy("obs")
  ws1 <- fake_ws(); s1 <- ShinySession$new(ws1)
  withReactiveDomain(s1, isolate(resume_server(spy)(s1$input, s1$output, s1)))
  token <- ws1$sent()[[1]]$config$resumeToken
  s1$wsClosed()
  key <- snapshot_token_key(token)

  clearCurrentAppState(); initCurrentAppState(shinyApp(fluidPage(textInput("b", "b")), srv))
  expect_message(res <- resume_real(resume_server(spy), list(token = token, dom = "intact", inputs = list())), "ui changed")
  expect_identical(res$info$resumed, "inputs")
  msgs <- res$ws$sent()
  expect_true(any(vapply(msgs, function(m) isTRUE(m$reload), logical(1))))
  expect_false(is.null(snapshot_store()$read(key)))   # kept for the reload path

  clearCurrentAppState(); initCurrentAppState(shinyApp(fluidPage(textInput("a", "a")), srv))
  expect_message(res <- resume_real(resume_server(spy), list(token = token, dom = "intact", inputs = list())), "Resumed \\(reconnect\\): restored")
  expect_identical(res$info$resumed, "snapshot")
})

test_that("a UI fingerprint that cannot be computed skips the UI check instead of raising", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  app <- shinyApp(fluidPage(), function(input, output, session) NULL)
  uiGone <- FALSE
  app$uiHash <- function() if (uiGone) stop("ui.R vanished") else "abc"
  initCurrentAppState(app); withr::defer(clearCurrentAppState())
  token <- closed_plus_session(new_spy("obs"))
  uiGone <- TRUE
  expect_message(
    res <- resume_real(resume_server(new_spy("obs")), list(token = token, dom = "intact", inputs = list())),
    "Resumed \\(reconnect\\): restored"
  )
  expect_identical(res$info$resumed, "snapshot")
})

test_that("inputs-only resume seeds an inactive RestoreContext on a real session", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  # With a bookmark store, the observers server.R creates before the resume
  # would run onRestore() callbacks for an active context.
  shinyOptions(bookmarkStore = "url")
  withr::defer(shinyOptions(bookmarkStore = NULL))
  spy <- new_spy("restore")
  server <- function(input, output, session) { session$onRestore(function(state) spy$restore <- spy$restore + 1) }
  expect_message(
    res <- resume_real(server, list(token = snapshot_token_new(), dom = "intact", inputs = list(t = "typed")),
                       before = function(s) s$createBookmarkObservers()),
    "from inputs only"
  )
  expect_false(res$session$restoreContext$active)
  expect_identical(res$session$restoreContext$input$get("t", force = TRUE), "typed")
  expect_equal(spy$restore, 0)
})

test_that("an adopted upload's directory moves to the adopting session and goes when it ends without a snapshot", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  wsA <- fake_ws(); a <- ShinySession$new(wsA)
  withReactiveDomain(a, isolate(resume_server(new_spy("obs"))(a$input, a$output, a)))
  job <- a$`@uploadInit`(list(list(name = "a.txt", size = 1, type = "text/plain")))
  op <- a$.__enclos_env__$private$fileUploadContext$getUploadOperation(job$jobId)
  op$fileBegin(); op$fileChunk(charToRaw("x")); op$fileEnd()
  withReactiveDomain(a, a$`@uploadEnd`(job$jobId, "file"))
  path <- isolate(a$input$file$datapath)
  expect_true(file.exists(path))
  token <- wsA$sent()[[1]]$config$resumeToken
  a$wsClosed()
  expect_true(file.exists(path))

  expect_message(
    res <- resume_real(resume_server(new_spy("obs")), list(token = token, dom = "intact", inputs = list(`file:shiny.file` = NULL))),
    "Resumed \\(reconnect\\): restored"
  )
  b <- res$session
  expect_identical(isolate(b$input$file$datapath), path)
  expect_true(file.exists(path))
  b$.endForGood()
  b$wsClosed()
  expect_false(file.exists(path))
})

test_that("session$close() tells the client not to reconnect and leaves no snapshot", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  key <- snapshot_token_key(s$.__enclos_env__$private$resumeToken)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  expect_true(s$.snapshotRegistry$writeNow())
  s$close()
  s$wsClosed()
  msgs <- ws$sent()
  expect_identical(msgs[[length(msgs)]]$allowReconnect, FALSE)
  expect_null(snapshot_store()$read(key))
})

test_that("session$close() closes the socket even when telling the client fails", {
  closed <- FALSE
  broken <- FALSE
  ws <- list(request = list(), send = function(msg) if (broken) stop("socket gone"), close = function() closed <<- TRUE)
  s <- ShinySession$new(ws)
  broken <- TRUE
  printed <- capture.output(type = "message", s$close())
  expect_match(paste(printed, collapse = "\n"), "socket gone")
  expect_true(closed)
})

test_that("a fatal unhandled error ends the session for good", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  key <- snapshot_token_key(s$.__enclos_env__$private$resumeToken)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  s$unhandledError(simpleError("boom"), close = TRUE)
  s$wsClosed()
  msgs <- ws$sent()
  expect_length(Filter(function(m) identical(m$allowReconnect, FALSE), msgs), 1)
  expect_identical(msgs[[length(msgs)]]$allowReconnect, FALSE)
  expect_null(snapshot_store()$read(key))
})

test_that("an observer that registers a data object re-runs after adoption, so its URL carries the new session", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  spy <- new_spy("runs")
  server <- function(input, output, session) {
    observe({
      spy$runs <- spy$runs + 1
      session$sendCustomMessage("url", session$registerDataObj("tbl", 1:3, function(data, req) NULL))
    })
  }
  ws <- fake_ws()
  s1 <- ShinySession$new(ws)
  withReactiveDomain(s1, isolate(server(s1$input, s1$output, s1)))
  flushReact()
  s1$wsClosed()
  reset_spies()
  expect_message(
    res <- resume_real(server, list(token = ws$sent()[[1]]$config$resumeToken, dom = "intact", inputs = list())),
    "Resumed \\(reconnect\\): restored"
  )
  expect_equal(spy$runs, 1)
  fallbacks <- res$session$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_match(fallbacks$reason, "uses a session URL")
})

# A websocket as httpuv hands it to createAppHandlers()$ws.
fake_httpuv_ws <- function() {
  ws <- fake_ws()
  ws$request <- list(PATH_INFO = "/websocket/")
  cb <- new.env()
  ws$onMessage <- function(f) cb$message <- f
  ws$onClose <- function(f) cb$close <- f
  ws$callbacks <- cb
  ws
}

test_that("with resume off, a session the server ends tells the client nothing, as on main", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  s$allowReconnect("force")
  n <- length(ws$sent())
  s$close()
  expect_length(ws$sent(), n)

  handlers <- createAppHandlers(NULL, function() function(input, output, session) stop("typo in server"))
  ws <- fake_httpuv_ws()
  handlers$ws(ws)
  capture.output(type = "message", expect_warning(
    expect_error(ws$callbacks$message(FALSE, '{"method":"init","data":{}}'), "typo in server"),
    "typo in server"
  ))
  expect_false(any(vapply(ws$sent(), function(m) identical(m$allowReconnect, FALSE), logical(1))))
  ws$callbacks$close()
})

test_that("an error escaping the server function on init or resume ends the session for good", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  server <- function(input, output, session) {
    v <- reactiveVal(1)
    stop("typo in server")
  }
  for (method in c("init", "resume")) {
    handlers <- createAppHandlers(NULL, function() server)
    ws <- fake_httpuv_ws()
    handlers$ws(ws)
    data <- if (method == "init") "{}" else sprintf('{"token":"%s","dom":"intact","inputs":{}}', strrep("a", 32))
    msg <- sprintf('{"method":"%s","data":%s}', method, data)
    capture.output(type = "message", expect_warning(expect_error(ws$callbacks$message(FALSE, msg), "typo in server"), "typo in server"))
    sent <- ws$sent()
    expect_identical(sent[[length(sent)]]$allowReconnect, FALSE, info = method)
    ws$callbacks$close()
    expect_null(snapshot_store()$read(snapshot_token_key(sent[[1]]$config$resumeToken)), info = method)
  }
})

test_that("with resume off, a `resume` from an older page is answered as `init` without reading saved state", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  store <- snapshot_store()
  key <- snapshot_token_key(token)
  expect_false(is.null(store$read(key)))
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_mocked_bindings(snapshot_store = function() stop("the store must not be read"))
  runs <- 0
  seen <- NULL
  sess <- NULL
  server <- function(input, output, session) {
    runs <<- runs + 1
    sess <<- session
    observe(seen <<- input$plus)
  }
  handlers <- createAppHandlers(NULL, function() server)
  ws <- fake_httpuv_ws()
  handlers$ws(ws)
  ws$callbacks$message(FALSE, sprintf(
    '{"method":"resume","data":{"token":"%s","dom":"intact","inputs":{"plus:shiny.action":3}}}', token
  ))
  flushReact()
  expect_identical(runs, 1)
  expect_equal(as.numeric(seen), 3)
  expect_null(sess$.snapshotRegistry)
  sent <- ws$sent()
  expect_null(sent[[1]]$config$resumeToken)
  expect_false(any(vapply(sent, function(m) !is.null(m$resumed), logical(1))))
  ws$callbacks$close()
  expect_false(is.null(store$read(key)))
})

test_that("a resume records its counts as an OTel log event carrying the session id", {
  withr::local_envvar(SHINY_RESUME = "TRUE")
  local_snapshot_store_dir()
  token <- closed_plus_session(new_spy("obs"))
  logged <- list()
  local_mocked_bindings(
    has_otel_collect = function(collect) identical(collect, "session"),
    otel_log = function(msg, ..., attributes = NULL, severity = "info", logger = NULL) {
      logged[[length(logged) + 1L]] <<- list(msg = msg, attributes = attributes)
    }
  )
  expect_message(
    res <- resume_real(resume_server(new_spy("obs")), list(token = token, dom = "intact", inputs = list())),
    "Resumed \\(reconnect\\): restored"
  )
  event <- Filter(function(l) identical(l$msg, "Resume"), logged)
  expect_length(event, 1)
  attrs <- event[[1]]$attributes
  expect_identical(attrs$session.id, res$session$token)
  expect_identical(attrs$shiny.resume.from, "reconnect")
  expect_identical(attrs$shiny.resume.resumed, "snapshot")
  outcome <- res$session$.snapshotRegistry$adoptOutcome()
  expect_equal(attrs$shiny.resume.adopted, length(outcome$adopted))
  expect_gt(attrs$shiny.resume.adopted, 0)
  expect_equal(attrs$shiny.resume.reran, 0)
})

test_that("with the feature off a session has no registry, and every registry consumer copes", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  dir <- local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws)
  expect_null(s$.snapshotRegistry)
  expect_null(ws$sent()[[1]]$config$resumeToken)
  report <- s$resumeReport()
  expect_s3_class(report, "shiny_resume_report")
  expect_identical(nrow(report), 0L)
  expect_true(is.function(s$onResumed(function(info) NULL)))
  expect_error(s$onResumed("not a function"), "must be a function")
  s$allowReconnect(FALSE)
  expect_null(s$.snapshotRegistry)
  withReactiveDomain(s, isolate({ v <- reactiveVal(1); v(5) }))
  s$wsClosed()
  s$close()
  expect_length(list.files(dir, recursive = TRUE, include.dirs = TRUE), 0)

  m <- MockShinySession$new()
  expect_null(m$.snapshotRegistry)
  expect_identical(nrow(m$resumeReport()), 0L)
  expect_true(is.function(m$onResumed(function(info) NULL)))
})

