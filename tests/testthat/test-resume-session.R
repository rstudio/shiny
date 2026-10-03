withr::local_envvar(SHINY_RESUME = "TRUE")

intact <- function(token, inputs) list(token = token, dom = "intact", inputs = inputs)
page_defaults <- list(`plus:shiny.action` = 0, txt = "", extra = "e", .clientdata_output_n_hidden = FALSE)

test_that("config carries a token and the reload setting with resume on, and neither with it off", {
  local_snapshot_store_dir()
  ws <- fake_ws(); s <- ShinySession$new(ws)
  local_cancel_writes(s)
  cfg <- ws$sent()[[1]]$config
  expect_match(cfg$resumeToken, "^[0-9a-f]{32}$")
  expect_identical(cfg$resumeReload, "ask")
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), {
    ws2 <- fake_ws(); s2 <- ShinySession$new(ws2)
    expect_null(ws2$sent()[[1]]$config$resumeToken)
    expect_null(s2$.snapshotRegistry)
  })
})

test_that("a valid token restores, sends `resumed` before any values, logs, deletes the record and writes its own", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  key <- snapshot_record_key(token)
  expect_false(is.null(snapshot_store()$read(key)))
  spy <- new_spy("obs", "txt")
  expect_message(
    res <- resume_real(counter_app(spy), intact(token, list(`plus:shiny.action` = 3, txt = "typed")), spy = spy),
    "Resumed \\(reconnect\\): restored 1 values and 2 inputs; 1 event handlers skipped, 0 re-ran"
  )
  expect_identical(res$outcome, "snapshot")
  expect_setequal(spy$received, c("plus", "txt"))     # onInputReceived fires on resume
  expect_equal(spy$obs, 0)
  expect_equal(spy$txt, 1)                             # a plain observer runs
  expect_equal(as.numeric(isolate(res$session$input$plus)), 3)
  msgs <- res$ws$sent()
  resumed <- sent_of(res$ws, "resumed")
  expect_length(resumed, 1)
  expect_identical(resumed[[1]], list(resumed = "snapshot"))
  firstValues <- which(vapply(msgs, function(m) !is.null(m$values), logical(1)))[1]
  expect_true(which(vapply(msgs, function(m) !is.null(m$resumed), logical(1))) < firstValues)
  expect_null(snapshot_store()$read(key))
  newToken <- msgs[[1]]$config$resumeToken
  expect_false(identical(newToken, token))
  expect_false(is.null(snapshot_store()$read(snapshot_record_key(newToken))))
})

test_that("changed live inputs apply after the first flush and their outputs reach the client", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  spy <- new_spy("obs", "txt")
  res <- resume_real(counter_app(spy), intact(token, list(`plus:shiny.action` = 4, txt = "typed", .clientdata_output_n_hidden = FALSE)))
  expect_equal(spy$obs, 0)
  expect_false(res$session$.snapshotRegistry$restoring())
  expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
  live_flush(res$session)
  expect_equal(spy$obs, 1)
  values <- sent_of(res$ws, "values")
  expect_identical(values[[length(values)]]$values$n, "4")
})

test_that("a fresh page keeps the record's inputs over widget defaults and pushes the differing ones", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  spy <- new_spy("obs", "txt")
  res <- resume_real(counter_app(spy), list(token = token, dom = "fresh", inputs = page_defaults))
  s <- res$session
  expect_equal(as.numeric(isolate(s$input$plus)), 3)
  expect_identical(isolate(s$input$txt), "typed")
  expect_identical(isolate(s$input$extra), "e")
  expect_equal(spy$obs, 0)
  resumed <- sent_of(res$ws, "resumed")[[1]]
  expect_identical(resumed$resumed, "snapshot")
  expect_setequal(names(resumed$inputs), c("plus", "txt"))   # not `extra`, not clientData
  expect_equal(resumed$inputs$plus, 3)
  # A page that already shows `txt` only differs in the button.
  res2 <- resume_real(counter_app(new_spy("obs", "txt")),
    list(token = closed_counter_session(), dom = "fresh", inputs = utils::modifyList(page_defaults, list(txt = "typed"))))
  expect_identical(names(sent_of(res2$ws, "resumed")[[1]]$inputs), "plus")
})

test_that("on a fresh page a gate trip still seeds and pushes the record's inputs", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  changed <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    added <- reactiveVal(1)
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
    output$n <- renderText(count())
  }}
  spy <- new_spy("obs")
  expect_message(res <- resume_real(changed(spy), list(token = token, dom = "fresh", inputs = page_defaults)),
                 "from inputs only: \\|reactiveVal\\|added \\(no saved value\\)")
  expect_identical(res$outcome, "inputs")
  resumed <- sent_of(res$ws, "resumed")[[1]]
  expect_identical(resumed$resumed, "inputs")
  expect_equal(resumed$inputs$plus, 3)
  expect_equal(as.numeric(isolate(res$session$input$plus)), 3)
  expect_equal(spy$obs, 1)                              # every handler ran
})

test_that("malformed, unknown and mismatched tokens fall through to inputs-only with the cause logged", {
  local_snapshot_store_dir()
  spy <- new_spy("obs", "txt")
  cases <- list(
    list(data = intact("../etc", list()), cause = "malformed token"),
    list(data = list(dom = "intact", inputs = list()), cause = "no token"),
    list(data = intact(snapshot_token_new(), list()), cause = "no saved state"),
    list(data = list(token = snapshot_token_new(), dom = "sideways", inputs = list()), cause = "unknown dom")
  )
  for (case in cases) {
    expect_message(res <- resume_real(counter_app(spy), case$data),
                   paste0("Resumed \\(reconnect\\) from inputs only: ", case$cause))
    expect_identical(res$outcome, "inputs")
    expect_identical(sent_of(res$ws, "resumed")[[1]], list(resumed = "inputs"))
  }
  token <- closed_counter_session()
  local_enable_resume(appVersion = "v2")
  expect_message(resume_real(counter_app(spy), intact(token, list())), "from inputs only: appVersion differs")
})

test_that("a record of another format falls through to inputs-only", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  store <- snapshot_store(); key <- snapshot_record_key(token)
  rec <- store$read(key)
  old <- rec; old$identity$formatVersion <- 1L
  store$write(key, old)
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), intact(token, list())), "from inputs only: snapshot format version differs")
  store$write(key, list(identity = rec$identity, sources = "junk"))
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), intact(token, list())), "from inputs only: record unreadable")
  store$write(key, "not a list")
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), intact(token, list())), "from inputs only: record unreadable: not a list")
})

test_that("an incomplete record resumes from inputs only with its first blocked reason, and is deleted", {
  local_snapshot_store_dir()
  blocked_app <- function(input, output, session) {
    ok <- reactiveVal(1)
    conn <- reactiveVal(function() 1)
  }
  live <- live_session(blocked_app)
  token <- live_close(live)
  key <- snapshot_record_key(token)
  expect_false(snapshot_store()$read(key)$complete)
  expect_message(res <- resume_real(blocked_app, intact(token, list())),
                 "from inputs only: saved state incomplete: \\|reactiveVal\\|conn \\(value is not serializable: value\\)")
  expect_null(snapshot_store()$read(key))
})

test_that("a changed UI fingerprint on an intact page asks for a reload without running the server function; a fresh page skips the check", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  key <- snapshot_record_key(token)
  rec <- snapshot_store()$read(key); rec$identity$uiHash <- "stale"; snapshot_store()$write(key, rec)
  local_mocked_bindings(snapshot_ui_hash = function() "current")
  spy <- new_spy("obs", "txt")
  expect_message(res <- resume_real(counter_app(spy), intact(token, list(`plus:shiny.action` = 3, txt = "typed"))),
                 "^Resumed \\(reconnect\\): ui changed; reload requested")
  expect_identical(sent_of(res$ws, "reload")[[1]]$reload, TRUE)
  expect_length(sent_of(res$ws, "resumed"), 0)
  expect_identical(c(spy$obs, spy$txt), c(0, 0))
  expect_false(is.null(snapshot_store()$read(key)))
  expect_message(res2 <- resume_real(counter_app(new_spy("obs", "txt")), list(token = token, dom = "fresh", inputs = page_defaults)), "restored 1 values")
  expect_length(sent_of(res2$ws, "reload"), 0)
})

test_that("with bookmarking on, the session waiting for the reload still has a restore context", {
  local_snapshot_store_dir()
  shinyOptions(bookmarkStore = "url"); withr::defer(shinyOptions(bookmarkStore = NULL))
  token <- closed_counter_session()
  key <- snapshot_record_key(token)
  rec <- snapshot_store()$read(key); rec$identity$uiHash <- "stale"; snapshot_store()$write(key, rec)
  local_mocked_bindings(snapshot_ui_hash = function() "current")
  expect_message(res <- resume_real(counter_app(new_spy("obs", "txt")), intact(token, list()),
                                    before = function(s) s$createBookmarkObservers()),
                 "ui changed")
  expect_length(sent_of(res$ws, "fatalError"), 0)
})

test_that("inputs-only resume seeds an inactive RestoreContext; a fresh page without a record restores the URL's bookmark", {
  local_snapshot_store_dir()
  shinyOptions(bookmarkStore = "url"); withr::defer(shinyOptions(bookmarkStore = NULL))
  spy <- new_spy("restore")
  server <- function(input, output, session) {
    session$onRestore(function(state) spy$restore <- spy$restore + 1)
    restored <<- restoreInput("t", "default")
  }
  restored <- NULL
  res <- resume_real(server, intact(snapshot_token_new(), list(t = "typed", .clientdata_url_search = "?_inputs_&t=%22bm%22")),
                     before = function(s) s$createBookmarkObservers())
  expect_false(res$session$restoreContext$active)
  expect_identical(restored, "typed")
  expect_equal(spy$restore, 0)
  res2 <- resume_real(server, list(token = snapshot_token_new(), dom = "fresh", inputs = list(.clientdata_url_search = "?_inputs_&t=%22bm%22")),
                      before = function(s) s$createBookmarkObservers())
  expect_true(res2$session$restoreContext$active)
  expect_identical(restored, "bm")
  expect_equal(spy$restore, 1)
})

test_that("a fresh page without a record restores a bookmarked file; an intact page drops it", {
  local_snapshot_store_dir()
  stateDir <- withr::local_tempdir()
  saveRDS(list(), file.path(stateDir, "input.rds"))
  writeLines("bookmarked", file.path(stateDir, "0.txt"))
  shinyOptions(bookmarkStore = "server", load.interface = function(id, callback) callback(stateDir))
  withr::defer(shinyOptions(bookmarkStore = NULL, load.interface = NULL))
  inputs <- list(`f:shiny.file` = list(name = "a.txt", size = 11, type = "text/plain", datapath = "0.txt"),
                 .clientdata_url_search = "?_state_id_=abc")
  server <- function(input, output, session) NULL
  res <- resume_real(server, list(token = snapshot_token_new(), dom = "fresh", inputs = inputs))
  f <- isolate(res$session$input$f)
  expect_identical(f$name, "a.txt")
  expect_identical(readLines(f$datapath), "bookmarked")
  res2 <- resume_real(server, intact(snapshot_token_new(), inputs))
  expect_null(isolate(res2$session$input$f))
})

test_that("passwords are never saved and never pushed, and come back from the live client", {
  local_snapshot_store_dir()
  pw_app <- function(input, output, session) output$o <- renderText(paste(input$pw, input$user))
  live <- live_session(pw_app, outputs = "o", inputs = list(user = "bob"))
  withReactiveDomain(live$session, setSerializer("pw", serializerUnserializable))
  live_set(live$session, pw = "hunter2")
  token <- live_close(live)
  rec <- snapshot_store()$read(snapshot_record_key(token))
  expect_null(rec$inputs$pw)
  res <- resume_real(pw_app, intact(token, list(user = "bob", pw = "hunter2")))
  expect_identical(isolate(res$session$input$pw), "hunter2")
  res2 <- resume_real(pw_app, list(token = live_close(live_session(pw_app, inputs = list(user = "bob"))), dom = "fresh",
                                   inputs = list(user = "", pw = "")))
  expect_identical(names(sent_of(res2$ws, "resumed")[[1]]$inputs), "user")
  expect_identical(isolate(res2$session$input$pw), "")
})

test_that("an update during the first flush is not overwritten", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  s <- NULL
  res <- resume_real(counter_app(new_spy("obs", "txt")), intact(token, list(`plus:shiny.action` = 3, txt = "stale")),
    before = function(session) {
      s <<- session
      # The client's update arrives while the server is still restoring.
      session$onFlush(function() session$manageInputs(list(txt = "newest"), now = TRUE), once = TRUE)
    })
  expect_identical(isolate(s$input$txt), "newest")
})

test_that("session$reload() discards the saved state, stops writing and sends reload: fresh", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  s <- live$session
  s$.snapshotRegistry$writeNow()
  key <- s$.snapshotRegistry$storeKey()
  expect_false(is.null(snapshot_store()$read(key)))
  s$reload()
  expect_identical(sent_of(live$ws, "reload")[[1]]$reload, "fresh")
  expect_null(snapshot_store()$read(key))
  s$wsClosed()
  expect_null(snapshot_store()$read(key))             # the closing write was skipped
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), {
    off <- live_session(counter_app(new_spy("obs", "txt")))
    off$session$reload()
    expect_identical(sent_of(off$ws, "reload")[[1]]$reload, TRUE)
  })
})

test_that("@discardSnapshot deletes the record and stops further writes; @unload shortens the record's life", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  s <- live$session
  s$.snapshotRegistry$writeNow(); key <- s$.snapshotRegistry$storeKey()
  expect_true(s$`@discardSnapshot`())
  expect_null(snapshot_store()$read(key))
  s$wsClosed()
  expect_null(snapshot_store()$read(key))

  local_mocked_bindings(snapshot_unload_lifetime = function() 0.05)
  live2 <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  expect_true(live2$session$`@unload`())
  token <- live_close(live2)
  key2 <- snapshot_record_key(token)
  expect_false(is.null(snapshot_store()$read(key2)))   # the closing write still happened
  Sys.sleep(0.1); later::run_now()
  expect_null(snapshot_store()$read(key2))
})

test_that("allowReconnect(FALSE) is the per-session opt-out: still sent, no closing write, record deleted", {
  local_snapshot_store_dir()
  live <- live_session(function(input, output, session) { v <- reactiveVal(1) })
  s <- live$session
  s$.snapshotRegistry$writeNow()                      # written before the opt-out
  key <- s$.snapshotRegistry$storeKey()
  expect_false(is.null(snapshot_store()$read(key)))
  s$allowReconnect(FALSE)
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  expect_false(s$.snapshotRegistry$writerEnabled())
  s$wsClosed()
  expect_null(snapshot_store()$read(key))             # deleted at close, no closing write
  live2 <- live_session(function(input, output, session) { v <- reactiveVal(1); session$allowReconnect(TRUE) })
  expect_true(live2$session$.snapshotRegistry$writerEnabled())
})

test_that(".endForGood() deletes the record, tells the client once, and an error in the server function ends for good", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  s <- live$session
  s$.snapshotRegistry$writeNow(); key <- s$.snapshotRegistry$storeKey()
  s$.endForGood(); s$.endForGood()
  expect_null(snapshot_store()$read(key))
  expect_length(sent_of(live$ws, "allowReconnect"), 1)
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  s$wsClosed()
  expect_null(snapshot_store()$read(key))

  handlers <- createAppHandlers(NULL, function() function(input, output, session) stop("boom"))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  # printError() reports the app's error through warning(immediate. = TRUE).
  suppressWarnings(expect_error(ws$callbacks$message(FALSE, '{"method":"init","data":{}}'), "boom"))
  expect_identical(sent_of(ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
})

test_that("with resume off, a `resume` with a token is answered as init without reading saved state", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_mocked_bindings(snapshot_store = function() stop("the store must not be read"))
  runs <- 0; seen <- NULL
  server <- function(input, output, session) { runs <<- runs + 1; observe(seen <<- input$plus) }
  handlers <- createAppHandlers(NULL, function() server)
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","dom":"intact","inputs":{"plus:shiny.action":3}}}', token))
  flushReact()
  expect_identical(runs, 1)
  expect_equal(as.numeric(seen), 3)
  expect_length(sent_of(ws, "resumed"), 0)
})

test_that("the whole resume branch runs through createAppHandlers()", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  spy <- new_spy("obs", "txt")
  handlers <- createAppHandlers(NULL, function() counter_app(spy))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  # The log line is written at the first flush, which the app loop runs.
  expect_message(
    {
      ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","dom":"intact","inputs":{"plus:shiny.action":3,"txt":"typed"}}}', token))
      flushReact()
      flushPendingSessions()
    },
    "Resumed \\(reconnect\\): restored 1 values"
  )
  expect_equal(spy$obs, 0)
  expect_identical(sent_of(ws, "resumed")[[1]]$resumed, "snapshot")
  # Closing the socket cancels the session's pending debounced write.
  ws$callbacks$close()
})

test_that("wsClosed() forces a write that copies uploads beside the record, and a resume restores them", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "data.csv", "hello")
  token <- live_close(live)
  key <- snapshot_record_key(token)
  rec <- snapshot_store()$read(key)
  expect_match(rec$inputs$f$datapath, "^f-1-[0-9a-f]{8}\\.csv$")
  expect_true(file.exists(file.path(snapshot_store()$filesDir(key), rec$inputs$f$datapath)))
  res <- resume_real(up_app, list(token = token, dom = "fresh", inputs = list(`f:shiny.file` = NULL)))
  f <- isolate(res$session$input$f)
  expect_identical(readLines(f$datapath, warn = FALSE), "hello")
  expect_false(dir.exists(snapshot_store()$filesDir(key)))   # went with the record
})

test_that("a resume records its counts as an OTel log event carrying the session id", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  logged <- list()
  local_mocked_bindings(
    has_otel_collect = function(collect) identical(collect, "session"),
    otel_log = function(msg, ..., attributes = NULL, severity = "info", logger = NULL) {
      logged[[length(logged) + 1L]] <<- list(msg = msg, attributes = attributes)
    }
  )
  res <- resume_real(counter_app(new_spy("obs", "txt")), intact(token, list(`plus:shiny.action` = 3, txt = "typed")))
  event <- Filter(function(l) identical(l$msg, "Resume"), logged)
  expect_length(event, 1)
  attrs <- event[[1]]$attributes
  expect_identical(attrs$session.id, res$session$token)
  expect_identical(attrs$shiny.resume.from, "reconnect")
  expect_identical(attrs$shiny.resume.resumed, "snapshot")
  expect_identical(attrs$shiny.resume.values, 1L)
  expect_identical(attrs$shiny.resume.skipped, 1L)
})

test_that("the log line counts values, inputs, skipped and re-run handlers", {
  make <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    observeEvent(input$login, { token <<- 1 })
  }}
  token <- NULL
  rt <- snapshot_roundtrip(make(new_spy()), function(s) s$setInputs(plus = 1, login = 1))
  lines <- snapshot_resume_log("reconnect", rt$outcome)
  expect_identical(lines[[1]], "Resumed (reconnect): restored 1 values and 2 inputs; 1 event handlers skipped, 1 re-ran")
  expect_match(lines[[2]], "assigns closure variables")
  expect_identical(snapshot_resume_log("reload", list(resumed = "inputs", cause = "no saved state")),
                   "Resumed (reload) from inputs only: no saved state")
})
