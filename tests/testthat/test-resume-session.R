withr::local_envvar(SHINY_RESUME = "TRUE")

resume_data <- function(token, inputs) list(token = token, inputs = inputs)

test_that("config carries a token with resume on and none with it off", {
  local_snapshot_store_dir()
  ws <- fake_ws(); s <- ShinySession$new(ws)
  local_cancel_writes(s)
  cfg <- ws$sent()[[1]]$config
  expect_match(cfg$resumeToken, "^[0-9a-f]{32}$")
  expect_null(cfg$resumeReload)
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), {
    ws2 <- fake_ws(); s2 <- ShinySession$new(ws2)
    expect_null(ws2$sent()[[1]]$config$resumeToken)
    expect_null(s2$.snapshotRegistry)
  })
})

test_that("@unload deletes the record, cancels a pending write, and the closing write is skipped", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  s <- live$session
  s$.snapshotRegistry$writeNow(); key <- live_key(live)
  live_set(s, `plus:shiny.action` = 1)                # schedules a debounced write
  expect_true(s$`@unload`())
  expect_null(snapshot_store()$read(key))
  expect_false(s$.snapshotRegistry$writerEnabled())
  s$wsClosed()
  expect_null(snapshot_store()$read(key))
})

test_that("session$reload() reloads as on main and leaves the record to the page's unload", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  live$session$.snapshotRegistry$writeNow(); key <- live_key(live)
  live$session$reload()
  expect_identical(sent_of(live$ws, "reload")[[1]]$reload, TRUE)
  expect_false(is.null(snapshot_store()$read(key)))
})

test_that("wsClosed() forces a write that copies uploads beside the record", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "data.csv", "hello")
  token <- live_close(live)
  key <- snapshot_record_key(token)
  rec <- snapshot_store()$read(key)
  expect_match(rec$inputs$f$datapath, "^f-1-[0-9a-f]{8}\\.csv$")
  expect_true(file.exists(file.path(snapshot_store()$filesDir(key), rec$inputs$f$datapath)))
})

test_that("allowReconnect(FALSE) is the per-session opt-out: still sent, no closing write, record deleted", {
  local_snapshot_store_dir()
  live <- live_session(function(input, output, session) { v <- reactiveVal(1) })
  s <- live$session
  s$.snapshotRegistry$writeNow()                      # written before the opt-out
  key <- live_key(live)
  expect_false(is.null(snapshot_store()$read(key)))
  s$allowReconnect(FALSE)
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  expect_false(s$.snapshotRegistry$writerEnabled())
  s$wsClosed()
  expect_null(snapshot_store()$read(key))             # deleted at close, no closing write
  live2 <- live_session(function(input, output, session) { v <- reactiveVal(1); session$allowReconnect(TRUE) })
  expect_true(live2$session$.snapshotRegistry$writerEnabled())
})

error_app <- function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$boom, stop("kaboom"))
  output$n <- renderText(count())
}

crash <- function(live) {
  # printError() reports the error through warning(immediate. = TRUE).
  suppressWarnings(live_set(live$session, `boom:shiny.action` = 1))
  live
}

test_that(".endForGood() deletes the record, tells the client once, and skips the closing write", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  s <- live$session
  s$.snapshotRegistry$writeNow(); key <- live_key(live)
  s$.endForGood(); s$.endForGood()
  expect_null(snapshot_store()$read(key))
  expect_length(sent_of(live$ws, "allowReconnect"), 1)
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  s$wsClosed()
  expect_null(snapshot_store()$read(key))
})

test_that("session$close() and an error in the server function end for good", {
  local_snapshot_store_dir()
  live <- live_session(counter_app(new_spy("obs", "txt")), outputs = "n")
  live$session$.snapshotRegistry$writeNow(); key <- live_key(live)
  live$session$close()
  expect_true(live$ws$closed)
  expect_null(snapshot_store()$read(key))
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)

  handlers <- createAppHandlers(NULL, function() function(input, output, session) stop("boom"))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  suppressWarnings(expect_error(ws$callbacks$message(FALSE, '{"method":"init","data":{}}'), "boom"))
  expect_identical(sent_of(ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
})

test_that("a fatal observer error deletes the record, sends allowReconnect: false and closes, with no dialog", {
  local_snapshot_store_dir()
  live <- live_session(error_app, outputs = "n", inputs = list(`plus:shiny.action` = 0))
  live_set(live$session, `plus:shiny.action` = 1)
  live$session$.snapshotRegistry$writeNow(); key <- live_key(live)
  crash(live)
  expect_true(live$ws$closed)
  expect_null(snapshot_store()$read(key))
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  expect_length(sent_of(live$ws, "fatalError"), 0)
  live$session$wsClosed()
  expect_null(snapshot_store()$read(key))
})

test_that("with resume off a fatal error also ends for good", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  live <- live_session(error_app, outputs = "n")
  crash(live)
  expect_true(live$ws$closed)
  expect_identical(sent_of(live$ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
})

test_that("an onUnhandledError() callback that closes the session sends allowReconnect once", {
  local_snapshot_store_dir()
  live <- live_session(function(input, output, session) {
    session$onUnhandledError(function(e) session$close())
    observeEvent(input$boom, stop("kaboom"))
  })
  crash(live)
  expect_true(live$ws$closed)
  expect_length(sent_of(live$ws, "allowReconnect"), 1)
})

test_that("a valid token restores, sends `resumed` before any values, logs, deletes the record and writes its own", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  key <- snapshot_record_key(token)
  expect_false(is.null(snapshot_store()$read(key)))
  spy <- new_spy("obs", "txt")
  expect_message(
    res <- resume_real(counter_app(spy), resume_data(token, list(`plus:shiny.action` = 3, txt = "typed")), spy = spy),
    "^Resumed: restored 1 values and 2 inputs; 1 event handlers held back"
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

test_that("a payload value that differs from the record applies at the first flush without firing handlers", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  spy <- new_spy("obs", "txt")
  res <- resume_real(counter_app(spy), resume_data(token, list(`plus:shiny.action` = 4, txt = "typed", .clientdata_output_n_hidden = FALSE)))
  expect_equal(spy$obs, 0)
  expect_false(res$session$.snapshotRegistry$restoring())
  expect_equal(as.numeric(isolate(res$session$input$plus)), 4)
  values <- sent_of(res$ws, "values")
  expect_length(values, 1)                               # one render, already with the merged value
  expect_identical(values[[1]]$values$n, "3")            # count is state the handler did not touch
  live_flush(res$session)
  expect_equal(spy$obs, 0)
  expect_length(sent_of(res$ws, "values"), 1)
})

test_that("malformed, unknown and mismatched tokens fall through to inputs-only with the cause logged", {
  local_snapshot_store_dir()
  spy <- new_spy("obs", "txt")
  cases <- list(
    list(data = resume_data("../etc", list()), cause = "malformed token"),
    list(data = list(inputs = list()), cause = "no token"),
    list(data = resume_data(snapshot_token_new(), list()), cause = "no saved state")
  )
  for (case in cases) {
    expect_message(res <- resume_real(counter_app(spy), case$data),
                   paste0("^Resumed from inputs only: ", case$cause))
    expect_identical(res$outcome, "inputs")
    expect_identical(sent_of(res$ws, "resumed")[[1]], list(resumed = "inputs"))
  }
  token <- closed_counter_session()
  local_enable_resume(appVersion = "v2")
  expect_message(resume_real(counter_app(spy), resume_data(token, list())), "from inputs only: appVersion differs")
})

test_that("a record of another format falls through to inputs-only", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  store <- snapshot_store(); key <- snapshot_record_key(token)
  rec <- store$read(key)
  old <- rec; old$identity$formatVersion <- 1L
  store$write(key, old)
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())), "from inputs only: snapshot format version differs")
  store$write(key, list(identity = rec$identity, sources = "junk"))
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())), "from inputs only: record unreadable")
  store$write(key, "not a list")
  expect_message(resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())), "from inputs only: record unreadable: not a list")
})

test_that("changed R files without appVersion resume from inputs only before the server function runs", {
  local_snapshot_store_dir()
  dir <- local_app_dir()
  writeLines("v <- 1", file.path(dir, "app.R"))
  token <- closed_counter_session()
  writeLines("v <- 2", file.path(dir, "app.R"))
  expect_message(res <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())),
                 "from inputs only: app code changed")
  expect_identical(res$outcome, "inputs")
})

test_that("with an unchanged explicit appVersion, changed R files still restore", {
  local_snapshot_store_dir()
  dir <- local_app_dir()
  local_enable_resume(appVersion = "1")
  writeLines("v <- 1", file.path(dir, "app.R"))
  token <- closed_counter_session()
  writeLines("v <- 2", file.path(dir, "app.R"))
  expect_message(res <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())),
                 "^Resumed: restored 1 values")
  expect_identical(res$outcome, "snapshot")
})

test_that("non-R file changes without appVersion still restore", {
  local_snapshot_store_dir()
  dir <- local_app_dir()
  writeLines("v <- 1", file.path(dir, "app.R"))
  writeLines("a", file.path(dir, "data.csv"))
  token <- closed_counter_session()
  writeLines("b", file.path(dir, "data.csv"))
  expect_message(res <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())),
                 "^Resumed: restored 1 values")
})

test_that("decoding keeps NULL values and names the source that fails", {
  rec <- list(sources = list(a = list(bytes = serialize(NULL, NULL)), b = list(bytes = serialize(1, NULL))))
  out <- snapshot_decode_sources(rec)
  expect_true("value" %in% names(out$sources$a))
  expect_null(out$sources$a$value)
  expect_identical(out$sources$b$value, 1)
  expect_null(out$sources$b$bytes)                     # not held twice
  rec$sources$b$bytes <- as.raw(1:3)
  expect_error(snapshot_decode_sources(rec), "source `b` could not be decoded")
})

test_that("a record's `omitted` is optional but must be character", {
  rec <- list(identity = list(), complete = TRUE)
  expect_null(snapshot_shape_problem(rec))
  rec$omitted <- list("x")
  expect_identical(snapshot_shape_problem(rec), "`omitted` is not character")
})

test_that("a saved value that cannot be decoded resumes from inputs only before the server function runs", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  store <- snapshot_store(); key <- snapshot_record_key(token)
  rec <- store$read(key)
  rec$sources[["|reactiveVal|count"]]$bytes <- as.raw(c(1, 2, 3))
  store$write(key, rec)
  runs <- 0
  app <- function(input, output, session) {
    runs <<- runs + 1
    counter_app(new_spy("obs", "txt"))(input, output, session)
  }
  expect_message(res <- resume_real(app, resume_data(token, list())),
                 "from inputs only: record unreadable: source `\\|reactiveVal\\|count` could not be decoded")
  expect_identical(res$outcome, "inputs")
  expect_identical(runs, 1)
  expect_null(store$read(key))
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
  expect_message(res <- resume_real(blocked_app, resume_data(token, list())),
                 "from inputs only: saved state incomplete: \\|reactiveVal\\|conn \\(value is not serializable: value\\)")
  expect_null(snapshot_store()$read(key))
})

test_that("passwords are never saved and come back from the live client", {
  local_snapshot_store_dir()
  pw_app <- function(input, output, session) output$o <- renderText(paste(input$pw, input$user))
  live <- live_session(pw_app, outputs = "o", inputs = list(user = "bob"))
  withReactiveDomain(live$session, setSerializer("pw", serializerUnserializable))
  live_set(live$session, pw = "hunter2")
  token <- live_close(live)
  rec <- snapshot_store()$read(snapshot_record_key(token))
  expect_null(rec$inputs$pw)
  res <- resume_real(pw_app, resume_data(token, list(user = "bob", pw = "hunter2")))
  expect_identical(isolate(res$session$input$pw), "hunter2")
})

test_that("an update during the first flush is not overwritten", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  s <- NULL
  res <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list(`plus:shiny.action` = 3, txt = "stale")),
    before = function(session) {
      s <<- session
      # The client's update arrives while the server is still restoring.
      session$onFlush(function() session$manageInputs(list(txt = "newest"), now = TRUE), once = TRUE)
    })
  expect_identical(isolate(s$input$txt), "newest")
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
      ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","inputs":{"plus:shiny.action":3,"txt":"typed"}}}', token))
      flushReact()
      flushPendingSessions()
    },
    "^Resumed: restored 1 values"
  )
  expect_equal(spy$obs, 0)
  expect_identical(sent_of(ws, "resumed")[[1]]$resumed, "snapshot")
  # Closing the socket cancels the session's pending debounced write.
  ws$callbacks$close()
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
  app <- function(input, output, session) {
    counter_app(new_spy("obs", "txt"))(input, output, session)
    session$onResume(function(info) NULL)
  }
  res <- resume_real(app, resume_data(token, list(`plus:shiny.action` = 3, txt = "typed")))
  event <- Filter(function(l) identical(l$msg, "Resume"), logged)
  expect_length(event, 1)
  attrs <- event[[1]]$attributes
  expect_identical(attrs$session.id, res$session$token)
  expect_identical(attrs$shiny.resume.resumed, "snapshot")
  expect_identical(attrs$shiny.resume.values, 1L)
  expect_identical(attrs$shiny.resume.skipped, 1L)
  expect_null(attrs$shiny.resume.reran)
  expect_identical(attrs$shiny.resume.callbacks, 1L)
})

test_that("the log line counts restored values and inputs and held-back handlers", {
  make <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    observeEvent(input$login, { token <<- 1 })
  }}
  token <- NULL
  rt <- snapshot_roundtrip(make(new_spy()), function(s) s$setInputs(plus = 1, login = 1))
  expect_identical(snapshot_resume_log(rt$outcome),
                   "Resumed: restored 1 values and 2 inputs; 2 event handlers held back")
  expect_identical(snapshot_resume_log(list(resumed = "inputs", cause = "no saved state")),
                   "Resumed from inputs only: no saved state")
})

test_that("a resume restores uploads from beside the record, and they go with it", {
  local_snapshot_store_dir()
  up_app <- function(input, output, session) output$o <- renderText(input$f$name)
  live <- live_session(up_app, outputs = "o")
  live_upload(live$session, "f", "data.csv", "hello")
  token <- live_close(live)
  key <- snapshot_record_key(token)
  res <- resume_real(up_app, resume_data(token, list(`f:shiny.file` = NULL)))
  f <- isolate(res$session$input$f)
  expect_identical(readLines(f$datapath, warn = FALSE), "hello")
  expect_false(dir.exists(snapshot_store()$filesDir(key)))
})

test_that("an inputs-only resume seeds an inactive RestoreContext under each input's bare name", {
  local_snapshot_store_dir()
  shinyOptions(bookmarkStore = "url"); withr::defer(shinyOptions(bookmarkStore = NULL))
  spy <- new_spy("restore")
  restored <- NULL
  server <- function(input, output, session) {
    session$onRestore(function(state) spy$restore <- spy$restore + 1)
    restored <<- list(t = restoreInput("t", "default"), n = restoreInput("n", 10))
  }
  res <- resume_real(server,
    resume_data(snapshot_token_new(), list(t = "typed", `n:shiny.number` = 42, .clientdata_url_search = "?_inputs_&t=%22bm%22")),
    before = function(s) s$createBookmarkObservers())
  expect_false(res$session$restoreContext$active)
  expect_identical(restored$t, "typed")
  expect_identical(restored$n, 42)
  expect_equal(spy$restore, 0)
})

test_that("an inputs-only resume holds back event handlers, so a replayed click does not fire", {
  local_snapshot_store_dir()
  spy <- new_spy("obs", "txt")
  expect_message(res <- resume_real(counter_app(spy), resume_data(snapshot_token_new(), list(`plus:shiny.action` = 3, txt = "typed"))),
                 "from inputs only: no saved state")
  expect_equal(spy$obs, 0)
  expect_equal(spy$txt, 1)
  live_set(res$session, `plus:shiny.action` = 4)
  expect_equal(spy$obs, 1)
})

test_that("two resumes with one token (a duplicated tab): the first gets the record, the second inputs only", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  expect_message(first <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())), "^Resumed: restored")
  expect_message(second <- resume_real(counter_app(new_spy("obs", "txt")), resume_data(token, list())),
                 "^Resumed from inputs only: no saved state")
  expect_identical(first$outcome, "snapshot")
  expect_identical(second$outcome, "inputs")
})

test_that("a `dom` key from an older client is ignored", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  expect_message(res <- resume_real(counter_app(new_spy("obs", "txt")), list(token = token, dom = "fresh", inputs = list())),
                 "^Resumed: restored 1 values")
  expect_length(sent_of(res$ws, "resumed")[[1]], 1)    # no `inputs` push
})

test_that("with resume off, a `resume` with a token is answered as init without reading saved state", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_mocked_bindings(snapshot_store = function() stop("the store must not be read"))
  runs <- 0; seen <- NULL
  runs2 <- list2env(list(n = 0))
  server <- function(input, output, session) {
    runs <<- runs + 1
    observe(seen <<- input$plus)
    observeEvent(input$plus, runs2$n <- runs2$n + 1)
  }
  handlers <- createAppHandlers(NULL, function() server)
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","inputs":{"plus:shiny.action":3}}}', token))
  flushReact()
  expect_identical(runs, 1)
  expect_equal(as.numeric(seen), 3)
  expect_length(sent_of(ws, "resumed"), 0)
  expect_identical(runs2$n, 0)
})

test_that("snapshot_undefined_outputs() lists the page's outputs the server did not define", {
  cd <- list(output_a_hidden = FALSE, output_b_hidden = TRUE, url_search = "", `output_mod-c_hidden` = FALSE)
  expect_identical(snapshot_undefined_outputs(cd, c("a", "mod-c")), "b")
  expect_identical(snapshot_undefined_outputs(list(), "a"), character(0))
})

test_that("a resumed session logs outputs a held-back handler would have defined, under either outcome", {
  local_snapshot_store_dir()
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  app <- function(input, output, session) {
    count <- reactiveVal(0)
    output$top <- renderText("top")
    observeEvent(input$go, output$plot <- renderText("built in a handler"))
  }
  live <- live_session(app, outputs = "top", inputs = list(`go:shiny.action` = 0))
  live_set(live$session, `go:shiny.action` = 1)
  token <- live_close(live)
  page <- list(`go:shiny.action` = 1, .clientdata_output_top_hidden = FALSE, .clientdata_output_plot_hidden = FALSE)
  expect_message(resume_real(app, resume_data(token, page)),
                 "Outputs on the page with no definition after resume: plot")
  expect_match(seen, "rule 3", all = FALSE)
  expect_match(seen, "session$onResume()", fixed = TRUE, all = FALSE)
  expect_message(resume_real(app, resume_data(snapshot_token_new(), page)),
                 "from inputs only: no saved state\nOutputs on the page with no definition after resume: plot")
  msgs <- character(0)
  withCallingHandlers(
    resume_real(app, resume_data(snapshot_token_new(), page[c(1, 2)])),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  expect_false(any(grepl("no definition", msgs)))
})

test_that("the log adds the undefined-outputs line only when there are some", {
  expect_identical(snapshot_resume_log(list(resumed = "inputs", cause = "x"), "p"),
                   c("Resumed from inputs only: x", "Outputs on the page with no definition after resume: p"))
  expect_identical(snapshot_resume_log(list(resumed = "inputs", cause = "x")), "Resumed from inputs only: x")
})

test_that("onResume() callbacks run before `resumed` is sent and before any observer, with restored = TRUE", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  spy <- new_spy("obs", "txt")
  ws <- fake_ws()
  seen <- NULL
  app <- function(input, output, session) {
    counter_app(spy)(input, output, session)
    session$onResume(function(info) {
      seen <<- list(info = info, resumedSent = length(sent_of(ws, "resumed")), txtRuns = spy$txt)
    })
  }
  expect_message(resume_real(app, resume_data(token, list(`plus:shiny.action` = 3, txt = "typed")), ws = ws),
                 "^Resumed: restored 1 values")
  expect_identical(seen, list(info = list(restored = TRUE), resumedSent = 0L, txtRuns = 0))
  expect_length(sent_of(ws, "resumed"), 1)
})

test_that("an inputs-only resume runs onResume() callbacks with restored = FALSE", {
  local_snapshot_store_dir()
  seen <- NULL
  app <- function(input, output, session) session$onResume(function(info) seen <<- info)
  expect_message(resume_real(app, resume_data(snapshot_token_new(), list())), "from inputs only: no saved state")
  expect_identical(seen, list(restored = FALSE))
})

test_that("an error in an onResume() callback ends the session for good and `resumed` is not sent", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  app <- function(input, output, session) {
    counter_app(new_spy("obs", "txt"))(input, output, session)
    session$onResume(function(info) stop("rebuild failed"))
  }
  handlers <- createAppHandlers(NULL, function() helper_defined(app))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  suppressWarnings(expect_error(
    ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","inputs":{}}}', token)),
    "rebuild failed"
  ))
  expect_length(sent_of(ws, "resumed"), 0)
  expect_identical(sent_of(ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  expect_null(snapshot_store()$read(snapshot_record_key(token)))
  expect_null(snapshot_store()$read(snapshot_record_key(ws$sent()[[1]]$config$resumeToken)))
})

test_that("a server function that errors on resume runs no onResume() callbacks and sends no `resumed`", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  ran <- FALSE
  app <- function(input, output, session) {
    session$onResume(function(info) ran <<- TRUE)
    stop("server boom")
  }
  handlers <- createAppHandlers(NULL, function() helper_defined(app))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  suppressWarnings(expect_error(
    ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","inputs":{}}}', token)),
    "server boom"
  ))
  expect_false(ran)
  expect_length(sent_of(ws, "resumed"), 0)
})

test_that("a source with no saved value ends the session for good after the server function, with nothing saved", {
  local_snapshot_store_dir()
  token <- closed_counter_session()
  ran <- FALSE
  app <- function(input, output, session) {
    counter_app(new_spy("obs", "txt"))(input, output, session)
    added <- reactiveVal("new")
    session$onResume(function(info) ran <<- TRUE)
  }
  handlers <- createAppHandlers(NULL, function() helper_defined(app))
  ws <- fake_httpuv_ws(); handlers$ws(ws)
  expect_message(
    suppressWarnings(expect_error(
      ws$callbacks$message(FALSE, sprintf('{"method":"resume","data":{"token":"%s","inputs":{}}}', token)),
      class = "shiny_resume_failed"
    )),
    "Resume failed after the server function ran: \\|reactiveVal\\|added \\(no saved value\\)"
  )
  expect_false(ran)
  expect_length(sent_of(ws, "resumed"), 0)
  expect_identical(sent_of(ws, "allowReconnect")[[1]]$allowReconnect, FALSE)
  newToken <- ws$sent()[[1]]$config$resumeToken
  ws$callbacks$close()
  expect_null(snapshot_store()$read(snapshot_record_key(token)))
  expect_null(snapshot_store()$read(snapshot_record_key(newToken)))
})

test_that("the first replacement record follows the first flush and holds the merged inputs", {
  local_snapshot_store_dir()
  app <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    late <- NULL
    observe({ if (is.null(late)) late <<- reactiveVal(5, label = "late") })
  }
  live <- live_session(app, inputs = list(`plus:shiny.action` = 0))
  for (i in 1:3) live_set(live$session, `plus:shiny.action` = i)
  token <- live_close(live)
  res <- resume_real(app, resume_data(token, list(`plus:shiny.action` = 4)))
  newToken <- res$ws$sent()[[1]]$config$resumeToken
  rec <- snapshot_store()$read(snapshot_record_key(newToken))
  expect_true("|reactiveVal|late" %in% names(rec$sources))   # created in the first flush
  expect_equal(as.numeric(rec$inputs$plus), 4)                # the merged inputs: the freshest baseline
})

test_that("an old session in this process cannot recreate a record a newer session claimed", {
  local_snapshot_store_dir()
  spy <- new_spy("obs", "txt")
  live <- live_session(counter_app(spy), outputs = "n")
  live$session$.snapshotRegistry$writeNow()
  key <- live_key(live)
  token <- live$ws$sent()[[1]]$config$resumeToken
  resume_real(counter_app(spy), resume_data(token, list()))
  expect_null(snapshot_store()$read(key))
  live$session$wsClosed()                               # the old socket's close arrives late
  expect_null(snapshot_store()$read(key))
})

test_that("a socket that opened and closed before any message leaves no record, so the retry resumes from inputs", {
  local_snapshot_store_dir()
  ws <- fake_ws()
  s <- ShinySession$new(ws); local_cancel_writes(s)
  tb <- ws$sent()[[1]]$config$resumeToken
  s$wsClosed()
  expect_null(snapshot_store()$read(snapshot_record_key(tb)))
  app <- function(input, output, session) { count <- reactiveVal(0); output$n <- renderText(count()) }
  expect_message(res <- resume_real(app, list(token = tb, inputs = list())), "from inputs only: no saved state")
  expect_identical(res$outcome, "inputs")
})

test_that("snapshot_preflight() names each cause without touching the session", {
  local_snapshot_store_dir()
  s <- ShinySession$new(fake_ws()); local_cancel_writes(s)
  expect_equal(snapshot_preflight(list(), s)$cause, "no token")
  expect_equal(snapshot_preflight(list(token = "zz"), s)$cause, "malformed token")
  expect_equal(snapshot_preflight(list(token = snapshot_token_new()), s)$cause, "no saved state")
  token <- closed_counter_session()
  pre <- snapshot_preflight(list(token = token), s)
  expect_true(pre$ok)
  expect_named(pre, c("ok", "record", "store", "key"))
  expect_equal(pre$record$sources[["|reactiveVal|count"]]$value, 3)
  expect_false(is.null(pre$store$read(pre$key)))
})

test_that("an input on the bookmark-exclude list is not saved and comes back from the client, modules included", {
  local_snapshot_store_dir()
  app <- function(input, output, session) {
    session$setBookmarkExclude("secret")
    moduleServer("m", function(input, output, session) session$setBookmarkExclude("key"))
    output$n <- renderText(paste(input$secret, input$`m-key`, input$keep))
  }
  live <- live_session(app, outputs = "n", inputs = list(secret = "s3cret", `m-key` = "k", keep = "yes"))
  report <- live$session$resumeReport()
  expect_match(report$reason[report$label == "secret"], "setBookmarkExclude")
  token <- live_close(live)
  rec <- snapshot_store()$read(snapshot_record_key(token))
  expect_false(is.null(rec))
  expect_setequal(names(rec$inputs), "keep")
  res <- suppressMessages(resume_real(app, list(token = token, inputs = list(secret = "s3cret", `m-key` = "k", keep = "yes"))))
  expect_identical(res$outcome, "snapshot")
  expect_identical(isolate(res$session$input$secret), "s3cret")
  expect_identical(isolate(res$session$input$`m-key`), "k")
})

test_that("with URL bookmarking and resume both on, a reconnect resumes from the record and the bookmark observers fire", {
  local_snapshot_store_dir()
  shinyOptions(bookmarkStore = "url"); withr::defer(shinyOptions(bookmarkStore = NULL))
  initCurrentAppState(NULL); withr::defer(clearCurrentAppState())
  spy <- new_spy("obs", "txt", "restore", "bookmarked")
  app <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
    observe({ input$txt; spy$txt <- spy$txt + 1 })
    session$onRestore(function(state) spy$restore <- spy$restore + 1)
    session$onBookmarked(function(url) spy$bookmarked <- spy$bookmarked + 1)
    output$n <- renderText(count())
  }
  live <- live_session(app, outputs = "n", inputs = list(`plus:shiny.action` = 0, txt = ""))
  for (i in 1:2) live_set(live$session, `plus:shiny.action` = i)
  live_set(live$session, txt = "typed")
  token <- live_close(live)
  res <- suppressMessages(resume_real(app,
    list(token = token, inputs = list(`plus:shiny.action` = 2, txt = "typed",
      .clientdata_url_protocol = "http:", .clientdata_url_hostname = "localhost",
      .clientdata_url_port = "80", .clientdata_url_pathname = "/")),
    before = function(s) s$createBookmarkObservers()))
  expect_identical(res$outcome, "snapshot")
  expect_identical(isolate(res$session$input$txt), "typed")
  expect_equal(spy$restore, 0)
  res$session$manageInputs(withReactiveDomain(res$session, applyInputHandlers(list(`._bookmark_` = 1L), res$session)))
  flushReact()
  expect_equal(spy$bookmarked, 1)
})
