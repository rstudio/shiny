withr::local_envvar(SHINY_RESUME = "TRUE")

counter_server <- function(spy) { force(spy); function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
  output$txt <- renderText({ spy$out <- spy$out + 1; paste("Count:", count()) })
}}

test_that("a counter comes back at 3, the handler is held back, the next click gives 4", {
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(counter_server(spy), function(s) for (i in 1:3) s$setInputs(plus = i))
  expect_identical(rt$outcome$resumed, "snapshot")
  s2 <- rt$second
  expect_equal(isolate(s2$env$count()), 3)
  expect_equal(spy$obs, 0)
  expect_equal(spy$out, 1)                               # outputs always render
  expect_equal(s2$getOutput("txt"), "Count: 3")
  s2$setInputs(plus = 4)
  expect_equal(isolate(s2$env$count()), 4)
  expect_equal(spy$obs, 1)
  expect_length(rt$outcome$skipped, 1)
  expect_match(rt$outcome$skipped, "input\\$plus")
})

test_that("a side-effect handler does not fire again; a conductor recomputes once on first read", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observeEvent(input$send, spy$emails <- spy$emails + 1)
    expensive <- reactive({ spy$runs <- spy$runs + 1; input$n * 100 })
    output$txt <- renderText(expensive())
  }}
  spy <- new_spy("emails", "runs")
  rt <- snapshot_roundtrip(make(spy), function(s) { s$setInputs(send = 1); s$setInputs(n = 2) })
  expect_equal(spy$emails, 0)
  expect_equal(spy$runs, 1)
  expect_equal(rt$second$getOutput("txt"), "200")
})

test_that("a plain observer runs again and double-applies; devmode names the source (rule 4)", {
  make <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    observe({ input$plus; isolate(count(count() + 1)) })
  }}
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  rt <- snapshot_roundtrip(make(new_spy()), function(s) for (i in 1:3) s$setInputs(plus = i))
  expect_equal(isolate(rt$second$env$count()), 4)
  expect_match(seen, "`count` was written during the first flush", all = FALSE)
})

test_that("a payload value the record lacks (record lag) converges at the first flush without firing its handler", {
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(counter_server(spy), function(s) for (i in 1:3) s$setInputs(plus = i),
                           live = list(plus = 4))
  expect_equal(spy$obs, 0)
  expect_equal(isolate(rt$second$env$count()), 3)       # state, not an event: the handler did not run
  expect_equal(as.numeric(isolate(rt$second$input$plus)), 4)
  expect_equal(spy$out, 1)                              # one render, with the merged value
})

test_that("a closure-assigning handler is held back; ignoreInit = TRUE handles the next event; once = TRUE may fire again", {
  make <- function(spy) { force(spy); function(input, output, session) {
    conn <- NULL
    observeEvent(input$login, { conn <<- "open"; spy$closure <- spy$closure + 1 })
    observeEvent(input$y, spy$ignore <- spy$ignore + 1, ignoreInit = TRUE)
    observeEvent(input$z, spy$once <- spy$once + 1, once = TRUE)
  }}
  spy <- new_spy("closure", "ignore", "once")
  rt <- snapshot_roundtrip(make(spy), function(s) { s$setInputs(login = 1, y = 1, z = 1) })
  expect_equal(spy$closure, 0)
  expect_null(rt$second$env$conn)                      # closure state is never saved
  expect_equal(spy$ignore, 0)
  expect_equal(spy$once, 0)
  rt$second$setInputs(y = 2, z = 2)
  expect_equal(spy$ignore, 1)
  expect_equal(spy$once, 1)
  expect_length(rt$outcome$skipped, 2)                 # login and z; ignoreInit skips y before the hold-back
  expect_null(rt$outcome$reran)
})

test_that("a source without a saved value ends the resume: no rollback, an error naming it", {
  v1 <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    session$userData$kept <- "from record"
  }
  v2 <- function(input, output, session) {
    count <- reactiveVal(0)
    added <- reactiveVal("new in v2")
    observeEvent(input$plus, count(count() + 1))
  }
  first <- snapshot_session(v1)
  for (i in 1:3) first$setInputs(plus = i)
  rec <- snapshot_decode_sources(first$.snapshotRegistry$snapshot()); rec$identity <- snapshot_identity(first)
  rec$omitted <- "|reactiveVal|other"                # only the ids it lists are spared
  s2 <- MockShinySession$new()
  srv <- capture_env(v2)
  expect_message(
    expect_error(
      s2$.snapshotRegistry$resume(rec, list(inputs = rec$inputs, clientData = list(), fileInputs = character(0)),
        function() withMockContext(s2, srv(s2$input, s2$output, s2))),
      "Saved state does not fit the app: \\|reactiveVal\\|added \\(no saved value\\)",
      class = "shiny_resume_failed"
    ),
    "^Resume failed after the server function ran: \\|reactiveVal\\|added \\(no saved value\\)\\. The session ended"
  )
  expect_equal(isolate(s2$env$count()), 3)              # no rollback
  expect_identical(s2$userData$kept, "from record")    # no userData rollback either
  expect_false(s2$.snapshotRegistry$restoring())
})

test_that("two sources sharing a label during the server function end the resume", {
  server <- function(input, output, session) {
    count <- reactiveVal(0, label = "count")
    observeEvent(input$plus, count(count() + 1))
  }
  twins <- function(input, output, session) {
    count <- reactiveVal(0, label = "count")
    other <- reactiveVal(0, label = "count")
  }
  first <- snapshot_session(server)
  first$setInputs(plus = 1)
  rec <- snapshot_decode_sources(first$.snapshotRegistry$snapshot()); rec$identity <- snapshot_identity(first)
  s2 <- MockShinySession$new()
  srv <- capture_env(twins)
  expect_message(
    expect_error(
      s2$.snapshotRegistry$resume(rec, list(inputs = rec$inputs, clientData = list(), fileInputs = character(0)),
        function() withMockContext(s2, srv(s2$input, s2$output, s2))),
      "duplicate label", class = "shiny_resume_failed"
    ),
    "Resume failed after the server function ran"
  )
  expect_equal(isolate(s2$env$count()), 1)              # the first keeps its restored value
})

# Resumes `v2` from a record `v1` wrote after `drive`, returning the second session.
resume_from <- function(v1, v2, drive = function(s) NULL) {
  first <- snapshot_session(v1)
  drive(first)
  rec <- snapshot_decode_sources(first$.snapshotRegistry$snapshot()); rec$identity <- snapshot_identity(first)
  s2 <- MockShinySession$new()
  srv <- capture_env(v2)
  s2$.snapshotRegistry$resume(rec, list(inputs = rec$inputs, clientData = list(), fileInputs = character(0)),
    function() withMockContext(s2, srv(s2$input, s2$output, s2)))
  s2
}

saved_ten <- function(input, output, session) { count <- reactiveVal(10, label = "count") }

test_that("excludeFromResume() around the constructor shows the constructor value during a resume", {
  s2 <- resume_from(saved_ten, function(input, output, session) {
    count <- excludeFromResume(reactiveVal(0, label = "count"))
    seen <- isolate(count())
  })
  expect_equal(s2$env$seen, 0)
  expect_equal(isolate(s2$env$count()), 0)
  s2$flushReact()
  expect_false("|reactiveVal|count" %in% names(s2$.snapshotRegistry$snapshot()$sources))
})

test_that("marking an existing source before its first use reverts it to the constructor value", {
  s2 <- resume_from(saved_ten, function(input, output, session) {
    count <- reactiveVal(0, label = "count")
    excludeFromResume(count)
    seen <- isolate(count())
  })
  expect_equal(s2$env$seen, 0)
  expect_equal(isolate(s2$env$count()), 0)
})

test_that("marking a source after the app read its restored value ends the resume", {
  expect_error(
    resume_from(saved_ten, function(input, output, session) {
      count <- reactiveVal(0, label = "count")
      seen <- isolate(count())
      excludeFromResume(count)
    }),
    "excludeFromResume() was called on `count` after the app used its restored value",
    fixed = TRUE, class = "shiny_resume_exclude_late"
  )
})

test_that("marking a source after the app wrote its restored value back ends the resume", {
  expect_error(
    resume_from(saved_ten, function(input, output, session) {
      count <- reactiveVal(0, label = "count")
      count(10)
      excludeFromResume(count)
    }),
    class = "shiny_resume_exclude_late"
  )
  v1 <- function(input, output, session) { rv <- reactiveValues(a = 10, .label = "rv") }
  expect_error(
    resume_from(v1, function(input, output, session) {
      rv <- reactiveValues(a = 0, .label = "rv")
      rv$a <- 10
      excludeFromResume(rv)
    }),
    class = "shiny_resume_exclude_late"
  )
})

test_that("marking a reactiveValues after reading its names ends the resume", {
  v1 <- function(input, output, session) { rv <- reactiveValues(a = 10, .label = "rv") }
  expect_error(
    resume_from(v1, function(input, output, session) {
      rv <- reactiveValues(a = 0, .label = "rv")
      isolate(names(rv))
      excludeFromResume(rv)
    }),
    class = "shiny_resume_exclude_late"
  )
  s2 <- resume_from(v1, function(input, output, session) {
    rv <- reactiveValues(a = 0, .label = "rv")
    excludeFromResume(rv)
  })
  expect_equal(isolate(s2$env$rv$a), 0)
})

test_that("a source read by one first-flush observer and excluded by another ends the session", {
  s2 <- resume_from(saved_ten, function(input, output, session) {
    count <- reactiveVal(0, label = "count")
    observe(count(), priority = 10)
    observe(excludeFromResume(count), priority = 0)
  })
  # A mock session does not re-raise observer errors: it closes the session
  # and calls onUnhandledError(), as a real session ends for good.
  seen <- NULL
  s2$onUnhandledError(function(e) seen <<- e)
  suppressWarnings(suppressMessages(s2$flushReact()))  # printError()'s report
  expect_s3_class(seen, "shiny_resume_exclude_late")
  expect_true(s2$isClosed())
})

test_that("after the resume finished, marking keeps the current value and omits it from later records", {
  s2 <- resume_from(saved_ten, function(input, output, session) {
    count <- reactiveVal(0, label = "count")
    observe(count())
  })
  s2$flushReact()
  expect_false(s2$.snapshotRegistry$restoring())
  excludeFromResume(s2$env$count)
  expect_equal(isolate(s2$env$count()), 10)
  expect_false("|reactiveVal|count" %in% names(s2$.snapshotRegistry$snapshot()$sources))
})

test_that("a source an observer excluded is listed as omitted and keeps its initial value on resume", {
  server <- function(input, output, session) {
    big <- reactiveVal(0, label = "big")
    count <- reactiveVal(0, label = "count")
    observeEvent(input$plus, count(count() + 1))
    observeEvent(input$drop, excludeFromResume(big))
  }
  rt <- snapshot_roundtrip(server, function(s) {
    s$setInputs(plus = 1)
    isolate(s$env$big(99))
    s$setInputs(drop = 1)
  })
  expect_identical(rt$record$omitted, "|reactiveVal|big")
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_equal(isolate(rt$second$env$big()), 0)
  expect_equal(isolate(rt$second$env$count()), 1)
})

test_that("a source excluded in session$onResume() does not end the next resume", {
  server <- function(input, output, session) {
    x <- reactiveVal(0, label = "x")
    count <- reactiveVal(0, label = "count")
    observeEvent(input$plus, count(count() + 1))
    session$onResume(function(info) excludeFromResume(x))
  }
  rt <- snapshot_roundtrip(server, function(s) { s$setInputs(plus = 1); isolate(s$env$x(5)) })
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_equal(isolate(rt$second$env$x()), 0)
  rec <- snapshot_decode_sources(rt$second$.snapshotRegistry$snapshot())
  expect_identical(rec$omitted, "|reactiveVal|x")
  s3 <- MockShinySession$new()
  srv <- capture_env(server)
  outcome <- s3$.snapshotRegistry$resume(rec, list(inputs = rec$inputs, clientData = list(), fileInputs = character(0)),
    function() withMockContext(s3, srv(s3$input, s3$output, s3)))
  expect_identical(outcome$resumed, "snapshot")
  expect_equal(isolate(s3$env$x()), 0)
  expect_equal(isolate(s3$env$count()), 1)
})

test_that("userData and module state under two namespaces come back", {
  mod <- function(id) moduleServer(id, function(input, output, session) {
    v <- reactiveVal(0)
    observeEvent(input$plus, v(v() + 1))
    v
  })
  server <- function(input, output, session) {
    a <- mod("a"); b <- mod("b")
    session$userData$note <- "kept"
  }
  rt <- snapshot_roundtrip(server, function(s) { s$setInputs(`a-plus` = 1); s$setInputs(`a-plus` = 2); s$setInputs(`b-plus` = 1) })
  expect_equal(isolate(rt$second$env$a()), 2)
  expect_equal(isolate(rt$second$env$b()), 1)
  expect_identical(rt$second$userData$note, "kept")
})

test_that("a source created by an observer at the first flush applies its record without tripping", {
  server <- function(input, output, session) {
    late <- NULL
    observe({ if (is.null(late)) late <<- reactiveVal(0, label = "late") })
  }
  rt <- snapshot_roundtrip(server, function(s) { s$flushReact(); isolate(s$env$late(7)) })
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_equal(isolate(rt$second$env$late()), 7)
  expect_identical(rt$outcome$values, 1L)              # counted after the first flush, not at verify
})

test_that("Shiny's own internal sources do not trip the gate on resume", {
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    slow <- debounce(reactive(input$plus), 50)
    fast <- throttle(reactive(input$plus), 50)
    ExtendedTask$new(function() 1)
    output$txt <- renderText(paste(count(), slow(), fast()))
  }
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:2) s$setInputs(plus = i))
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_null(rt$outcome$cause)
  expect_equal(isolate(rt$second$env$count()), 2)
})

test_that("internal sources that share a label do not trip the gate or block the record", {
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    plus <- reactive(input$plus)
    both <- lapply(1:2, function(i) debounce(plus, 50))
    a <- debounce(plus, 50); a <- debounce(plus, 50)
    ExtendedTask$new(function() 1)
    ExtendedTask$new(function() 2)
    output$txt <- renderText(paste(count(), both[[1]](), both[[2]](), a()))
  }
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:2) s$setInputs(plus = i))
  expect_true(rt$record$complete)
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_null(rt$outcome$cause)
  expect_equal(isolate(rt$second$env$count()), 2)
})

test_that("rule 4 is not raised for writes to sources that were not restored", {
  server <- function(input, output, session) {
    secret <- reactiveVal(0); excludeFromResume(secret)
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    slow <- debounce(reactive(input$plus), 50)
    observe({ input$plus; isolate(secret(secret() + 1)) })
    output$txt <- renderText(paste(count(), slow()))
  }
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:2) s$setInputs(plus = i))
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_false(any(grepl("first flush", seen)))
})

test_that("userData is restored before the server function runs, so a top-level guard sees it", {
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    if (is.null(session$userData$visits)) session$userData$visits <- 0
    session$userData$seenAtStart <- session$userData$visits
    observeEvent(input$plus, session$userData$visits <- session$userData$visits + 1)
  }
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:2) s$setInputs(plus = i))
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_identical(rt$second$userData$seenAtStart, 2)
  expect_identical(rt$second$userData$visits, 2)
})

test_that("saved values nothing recreates are listed and logged, and the resume still succeeds", {
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  panel <- function(id) moduleServer(id, function(input, output, session) {
    n <- reactiveVal(0)
    observeEvent(input$plus, n(n() + 1))
  })
  server <- function(input, output, session) observeEvent(input$add, panel("p1"))
  rt <- snapshot_roundtrip(server, function(s) { s$setInputs(add = 1); s$setInputs(`p1-plus` = 1) })
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_identical(rt$outcome$unclaimed, "p1-|reactiveVal|n")
  expect_match(snapshot_resume_log(rt$outcome),
               "^Saved values not restored because nothing recreated them: p1-\\|reactiveVal\\|n$", all = FALSE)
  expect_match(seen, "rule 3", all = FALSE)
})

test_that("a source excluded after claiming its record is not listed as unclaimed", {
  rt <- snapshot_roundtrip(
    function(input, output, session) { count <- reactiveVal(10, label = "count") },
    resume_server = function(input, output, session) { count <- excludeFromResume(reactiveVal(0, label = "count")) }
  )
  expect_identical(rt$outcome$unclaimed, character(0))
})

test_that("an excluded userData name in an older record is not restored; others are", {
  first <- snapshot_session(function(input, output, session) {
    session$userData$token <- "secret"
    session$userData$note <- "draft"
  })
  rec <- snapshot_decode_sources(first$.snapshotRegistry$snapshot())
  local_enable_resume(userDataExclude = "token")
  s2 <- MockShinySession$new()
  seen <- NULL
  s2$.snapshotRegistry$resume(rec, list(inputs = list(), clientData = list(), fileInputs = character(0)),
    function() seen <<- exists("token", envir = s2$userData, inherits = FALSE))
  expect_false(seen)                                    # absent while the server function runs
  expect_identical(s2$userData$note, "draft")
})
