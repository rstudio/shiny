test_that("startHttpuvApp() announces the URL only after the socket is bound (#4400)", {
  # The "Listening on ..." message is used as a readiness signal (e.g. by
  # shinytest2 and log watchers), so it must not be emitted until httpuv has
  # actually bound the listening socket. Here we mock the (synchronous) bind and
  # confirm the message is printed *after* it returns, never before.
  withr::defer(handlerManager$clear())

  app <- shinyApp(fluidPage(), function(input, output, session) {})

  bound <- FALSE
  bound_when_announced <- NULL

  # Replace httpuv's startServer so no real socket is opened; record the bind.
  local_mocked_bindings(
    startServer = function(host, port, app, ...) {
      bound <<- TRUE
      structure(list(), class = "mock_server")
    }
  )

  server <- withCallingHandlers(
    startHttpuvApp(app, port = 3839L, host = "127.0.0.1", quiet = FALSE),
    message = function(m) {
      if (grepl("Listening on", conditionMessage(m), fixed = TRUE)) {
        bound_when_announced <<- bound
      }
      invokeRestart("muffleMessage")
    }
  )

  # The bind was attempted and its return value is passed through.
  expect_true(bound)
  expect_s3_class(server, "mock_server")

  # The "Listening on" line was seen, and only after the bind had returned.
  expect_true(isTRUE(bound_when_announced))
})

test_that("startHttpuvApp() with quiet = TRUE binds without announcing (#4400)", {
  withr::defer(handlerManager$clear())

  app <- shinyApp(fluidPage(), function(input, output, session) {})

  bound <- FALSE
  local_mocked_bindings(
    startServer = function(host, port, app, ...) {
      bound <<- TRUE
      structure(list(), class = "mock_server")
    }
  )

  expect_no_message(
    server <- startHttpuvApp(app, port = 3839L, host = "127.0.0.1", quiet = TRUE)
  )
  expect_true(bound)
  expect_s3_class(server, "mock_server")
})

test_that("flushPendingSessions() closes only the session whose flush errors (#4439)", {
  # Without the error handler, the error escapes serviceApp() and stops
  # runApp(), which ends every session. So the healthy session must still
  # be flushed and left open.
  flushed <- character()
  closed <- character()
  fake_session <- function(name, flush) {
    list(
      flushOutput = function() {
        flushed <<- c(flushed, name)
        flush()
      },
      close = function() closed <<- c(closed, name)
    )
  }
  appsNeedingFlush$set("test-flush-good", fake_session("good", function() NULL))
  appsNeedingFlush$set("test-flush-bad", fake_session("bad", function() stop("boom")))
  withr::defer({
    appsNeedingFlush$remove("test-flush-good")
    appsNeedingFlush$remove("test-flush-bad")
  })
  # The bad session must be flushed first, or the escaping error would not
  # stop the good one from being flushed.
  ours <- intersect(appsNeedingFlush$keys(), c("test-flush-bad", "test-flush-good"))
  expect_equal(ours, c("test-flush-bad", "test-flush-good"))

  expect_no_error(flushPendingSessions())
  expect_setequal(flushed, c("bad", "good"))
  expect_equal(closed, "bad")
})

test_that("ShinySession$dispatch() reports an unknown method once (#4439)", {
  responses <- list()
  env <- new.env()
  env$self <- list(`@known` = function(x) x * 2)
  env$private <- list(
    sendErrorResponse = function(msg, error) {
      responses[[length(responses) + 1]] <<- list(error = error)
    },
    sendResponse = function(msg, value) {
      responses[[length(responses) + 1]] <<- list(value = value)
    }
  )
  dispatch <- ShinySession$public_methods$dispatch
  environment(dispatch) <- env

  dispatch(list(method = "nope", args = list(), tag = 1))
  expect_equal(responses, list(list(error = "Unknown method nope")))

  responses <- list()
  dispatch(list(method = "known", args = list(3), tag = 2))
  expect_equal(responses, list(list(value = 6)))
})

test_that("a websocket message whose method isn't a single string is rejected (#4439)", {
  server_runs <- 0
  handlers <- createAppHandlers(NULL, function() {
    function(input, output) server_runs <<- server_runs + 1
  })

  on_message <- NULL
  handlers$ws(list(
    request = list(),
    send = function(json) invisible(NULL),
    close = function() invisible(NULL),
    onMessage = function(f) on_message <<- f,
    onClose = function(f) invisible(NULL)
  ))
  withr::defer({
    for (token in appsByToken$keys()) appsByToken$remove(token)
  })

  # withLogErrors() logs the error as a warning, then rethrows it
  send <- function(json) suppressWarnings(on_message(FALSE, json))

  send('{"method":"init","data":{}}')
  expect_equal(server_runs, 1)

  # A number would pick `init` by position in `switch()`
  expect_error(send('{"method":1,"data":{}}'), "single string")
  expect_error(send('{"method":["a","b"],"data":{}}'), "single string")
  expect_equal(server_runs, 1)
})

test_that("decodeMessage() gives no blobs for a binary frame with only JSON (#4439)", {
  frame <- function(...) {
    parts <- lapply(list(...), charToRaw)
    c(
      writeBin(0x01020202L, raw(), endian = "little"),
      unlist(lapply(parts, function(p) {
        c(writeBin(length(p), raw(), endian = "little"), p)
      }))
    )
  }

  msg <- decodeMessage(frame('{"method":"m","args":[]}'))
  expect_equal(msg$method, "m")
  expect_length(msg$blobs, 0)

  msg <- decodeMessage(frame('{"method":"m","args":[]}', "ab"))
  expect_equal(msg$blobs, list(charToRaw("ab")))

  # Short UTF-8 JSON messages (< 4 bytes, e.g. "{}") should decode without subscript error
  expect_equal(decodeMessage(charToRaw("{}")), empty_named_list())

  # Truncated or malformed binary messages should error cleanly instead of hanging or reading out of bounds
  magic <- as.raw(c(0x02, 0x02, 0x02, 0x01))
  expect_error(decodeMessage(magic), "Empty binary message")
  expect_error(decodeMessage(c(magic, as.raw(c(0x01, 0x02)))), "Truncated binary message header")
  # Negative length (-4L = 0xfc 0xff 0xff 0xff) previously caused an infinite loop
  expect_error(
    decodeMessage(c(magic, as.raw(c(0xfc, 0xff, 0xff, 0xff)))),
    "Invalid binary message part length"
  )
})
