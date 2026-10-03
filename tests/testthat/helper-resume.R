# Points the process store at a fresh temp cache dir for the calling test.
local_snapshot_store_dir <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(R_USER_CACHE_DIR = dir, .local_envir = envir)
  snapshot_store_reset()
  withr::defer(snapshot_store_reset(), envir = envir)
  dir
}

# Calls enableResume(...) for the calling test, as an app.R would; sessions
# created afterwards resolve it.
local_enable_resume <- function(..., .env = parent.frame()) {
  old <- getShinyOption("resume")
  withr::defer(shinyOptions(resume = old), envir = .env)
  enableResume(...)
  invisible()
}

file_mode <- function(path) as.character(file.info(path)$mode)

# Runs `server` in a fresh MockShinySession exactly as testServer() does and
# returns the session; the caller drives it and reads `session$.snapshotRegistry`.
snapshot_session <- function(server, session = MockShinySession$new()) {
  server <- capture_env(server)
  withMockContext(session, {
    server(input = session$input, output = session$output, session = session)
  })
  session
}

# Rewrites `server` so its body's environment becomes `session$env`, as
# testServer() does.
capture_env <- function(server) {
  body(server) <- rlang::expr({
    session$setEnv(base::environment())
    !!body(server)
  })
  server
}

# Bounded: a callback scheduled in the future cannot make it spin.
drain_later <- function() {
  for (i in seq_len(20)) {
    if (later::loop_empty()) break
    later::run_now(0)
  }
}

spy_registry <- new.env(parent = emptyenv())

new_spy <- function(...) {
  spy <- new.env(parent = emptyenv())
  for (nm in c(...)) assign(nm, 0, envir = spy)
  spy_registry$spies <- c(spy_registry$spies, spy)
  spy
}

reset_spies <- function() {
  for (spy in spy_registry$spies) {
    for (nm in ls(spy, all.names = TRUE)) {
      if (is.numeric(get(nm, envir = spy))) assign(nm, 0, envir = spy) else rm(list = nm, envir = spy)
    }
  }
}

# The R value a source record carries (records hold serialized bytes).
node_value <- function(record) unserialize(record$bytes)

# Ids of the sources with an identity, without the leading "namespace|".
short_ids <- function(session) sub("^[^|]*\\|", "", names(session$.snapshotRegistry$snapshot()$sources))

# Parses R code without source references, which is what an installed
# package's functions look like (no srcrefs, so no assignment-derived labels).
snapshot_eval_no_srcref <- function(text, env = parent.frame()) {
  exprs <- parse(text = text, keep.source = FALSE)
  for (e in exprs) eval(e, env)
  invisible(env)
}

# Runs `server` in a mock session, drives it, takes its record, resumes a
# second mock session from it (through serialize(), as the store would), and
# returns everything a test needs. `live` is what the returning client
# reports (default: what the record has).
snapshot_roundtrip <- function(server, drive = function(session) NULL, live = NULL, fresh = FALSE,
                               second = MockShinySession$new()) {
  first <- snapshot_session(server)
  drive(first)
  first$flushReact()
  drain_later()
  record <- first$.snapshotRegistry$snapshot()
  record$identity <- snapshot_identity(first)
  record <- unserialize(serialize(record, NULL))
  reset_spies()
  server <- capture_env(server)
  outcome <- second$.snapshotRegistry$resume(
    record,
    live = list(inputs = live %||% record$inputs, clientData = list(), fileInputs = character(0)),
    fresh = fresh,
    run = function() withMockContext(second, server(input = second$input, output = second$output, session = second))
  )
  # Changed live inputs apply after the first flush, so their dependents run in the second.
  second$flushReact()
  second$flushReact()
  list(first = first, second = second, record = record, outcome = second$.snapshotRegistry$outcome(),
       report = second$resumeReport())
}

# A real ShinySession schedules its debounced write on the process-wide timer
# queue. A session the test never closes would leave that timer pending for
# whichever test file runs next (test-timer.R compares the queue before and
# after), so cancel it when the calling test ends.
local_cancel_writes <- function(session, envir = parent.frame()) {
  withr::defer(
    if (!is.null(session$.snapshotRegistry)) session$.snapshotRegistry$pause(),
    envir = envir
  )
  invisible(session)
}

# An environment, not a list: ShinySession keeps a reference to it, so the
# test can read `ws$closed` after the session called `ws$close()`.
fake_ws <- function() {
  ws <- new.env(parent = emptyenv())
  ws$request <- list()
  ws$closed <- FALSE
  ws$messages <- list()
  ws$send <- function(msg) ws$messages[[length(ws$messages) + 1]] <- msg
  ws$close <- function() ws$closed <- TRUE
  ws$sent <- function() lapply(ws$messages, jsonlite::fromJSON, simplifyVector = FALSE)
  ws
}

# A stand-in for httpuv's WebSocket for createAppHandlers().
fake_httpuv_ws <- function() {
  ws <- fake_ws()
  ws$request <- list(PATH_INFO = "/websocket/")
  cb <- new.env()
  ws$onMessage <- function(f) cb$message <- f
  ws$onClose <- function(f) cb$close <- f
  ws$callbacks <- cb
  ws
}

# Starts one session for `server` and delivers `json` as its first message.
start_with <- function(server, json) {
  # Emulate runApp(): sessions copy shinyOptions() from the current app state
  # (ShinySession$initialize), which is how bookmarkStore reaches them.
  initCurrentAppState(NULL)
  on.exit(clearCurrentAppState(), add = TRUE)
  handlers <- createAppHandlers(NULL, function() server)
  ws <- fake_httpuv_ws()
  handlers$ws(ws)
  ws$callbacks$message(FALSE, json)
  flushReact()
  ws
}

# The messages carrying `type`, in the order sent.
sent_of <- function(ws, type) Filter(function(m) !is.null(m[[type]]), ws$sent())

# Drives a real ShinySession through the resume branch exactly as server.R
# does: `data` is the decoded `resume` payload.
resume_real <- function(server, data, ws = fake_ws(), spy = NULL, before = function(s) NULL,
                        envir = parent.frame()) {
  s <- ShinySession$new(ws)
  local_cancel_writes(s, envir)
  if (!is.null(spy)) s$onInputReceived(function(data) spy$received <- names(data))
  before(s)
  run <- function() withReactiveDomain(s, isolate(server(input = s$input, output = s$output, session = s)))
  outcome <- s$.resumeSession(data, run)
  flushReact(); s$flushOutput()
  list(session = s, outcome = outcome, ws = ws)
}

# A ShinySession over fake_ws() that ran `server`, with `outputs` visible and
# raw client `inputs` applied, after one flush.
live_session <- function(server, outputs = character(0), inputs = list(), ws = fake_ws(),
                         envir = parent.frame()) {
  s <- ShinySession$new(ws)
  local_cancel_writes(s, envir)
  withReactiveDomain(s, isolate(server(input = s$input, output = s$output, session = s)))
  visible <- stats::setNames(rep(list(FALSE), length(outputs)), sprintf(".clientdata_output_%s_hidden", outputs))
  s$manageInputs(c(withReactiveDomain(s, applyInputHandlers(inputs, s)), visible), now = TRUE)
  live_flush(s)
  list(session = s, ws = ws)
}

live_flush <- function(s) { flushReact(); s$flushOutput(); invisible(s) }

# Applies raw client inputs as server.R does for `update`, then flushes.
live_set <- function(s, ...) {
  s$manageInputs(withReactiveDomain(s, applyInputHandlers(list(...), s)), now = TRUE)
  live_flush(s)
}

# Closes the live session (its closing write runs) and returns its token.
live_close <- function(live) {
  live$session$wsClosed()
  live$ws$sent()[[1]]$config$resumeToken
}

counter_app <- function(spy) { force(spy); function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
  observe({ input$txt; spy$txt <- spy$txt + 1 })
  output$n <- renderText(count())
}}

# A closed session that clicked `plus` three times and typed into `txt`.
closed_counter_session <- function(spy = new_spy("obs", "txt")) {
  live <- live_session(counter_app(spy), outputs = "n", inputs = list(`plus:shiny.action` = 0, txt = ""))
  for (i in 1:3) live_set(live$session, `plus:shiny.action` = i)
  live_set(live$session, txt = "typed")
  live_close(live)
}

# Uploads `contents` as file `name` to `inputId` the way the client does.
live_upload <- function(s, inputId, name, contents) {
  job <- s$`@uploadInit`(list(list(name = name, size = nchar(contents), type = "text/plain")))
  op <- s$.__enclos_env__$private$fileUploadContext$getUploadOperation(job$jobId)
  op$fileBegin(); op$fileChunk(charToRaw(contents)); op$fileEnd()
  withReactiveDomain(s, s$`@uploadEnd`(job$jobId, inputId))
  live_flush(s)
}
