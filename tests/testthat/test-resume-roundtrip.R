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

test_that("an input that changed while disconnected fires its handler after the first flush, once", {
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(counter_server(spy), function(s) for (i in 1:3) s$setInputs(plus = i),
                           live = list(plus = 4))
  expect_equal(spy$obs, 1)
  expect_equal(isolate(rt$second$env$count()), 4)
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
