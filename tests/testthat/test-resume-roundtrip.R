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

test_that("on a fresh page, inputs the record has are not overwritten by widget defaults", {
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(counter_server(spy), function(s) for (i in 1:3) s$setInputs(plus = i),
                           live = list(plus = 0, extra = "e"), fresh = TRUE)
  expect_equal(as.numeric(isolate(rt$second$input$plus)), 3)
  expect_identical(isolate(rt$second$input$extra), "e")
  expect_equal(spy$obs, 0)
})

test_that("a hazard handler and a marked handler run; ignoreInit = TRUE handles the next event; once = TRUE may fire again", {
  make <- function(spy) { force(spy); function(input, output, session) {
    conn <- NULL
    observeEvent(input$login, { conn <<- "open"; spy$hazard <- spy$hazard + 1 })
    marked <- observeEvent(input$x, spy$marked <- spy$marked + 1)
    disableResume(marked)
    observeEvent(input$y, spy$ignore <- spy$ignore + 1, ignoreInit = TRUE)
    observeEvent(input$z, spy$once <- spy$once + 1, once = TRUE)
  }}
  spy <- new_spy("hazard", "marked", "ignore", "once")
  rt <- snapshot_roundtrip(make(spy), function(s) { s$setInputs(login = 1, x = 1, y = 1, z = 1) })
  expect_equal(spy$hazard, 1)
  expect_equal(spy$marked, 1)
  expect_equal(spy$ignore, 0)
  expect_equal(spy$once, 0)
  rt$second$setInputs(y = 2, z = 2)
  expect_equal(spy$ignore, 1)
  expect_equal(spy$once, 1)
  whys <- vapply(rt$outcome$reran, `[[`, "", "why")
  expect_setequal(whys, c("assigns closure variables", "disableResume()"))
})

test_that("a source without a saved value trips the gate: everything reverts and every handler runs", {
  v1 <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
  }}
  v2 <- function(spy) { force(spy); function(input, output, session) {
    count <- reactiveVal(0)
    added <- reactiveVal("new in v2")
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
  }}
  spy <- new_spy("obs")
  first <- snapshot_session(v1(spy))
  for (i in 1:3) first$setInputs(plus = i)
  rec <- first$.snapshotRegistry$snapshot(); rec$identity <- snapshot_identity(first)
  reset_spies()
  s2 <- MockShinySession$new()
  srv <- capture_env(v2(spy))
  out <- s2$.snapshotRegistry$resume(rec, list(inputs = rec$inputs, clientData = list(), fileInputs = character(0)), FALSE,
    function() withMockContext(s2, srv(s2$input, s2$output, s2)))
  expect_identical(out$resumed, "inputs")
  expect_match(out$cause, "\\|reactiveVal\\|added \\(no saved value\\)")
  expect_equal(isolate(s2$env$count()), 0)           # reverted, before anything ran
  s2$flushReact()
  expect_equal(spy$obs, 1)                             # the handler ran with the replayed input
  expect_equal(isolate(s2$env$count()), 1)
  expect_equal(as.numeric(isolate(s2$input$plus)), 3) # inputs stay seeded
})

test_that("a record that fails to apply trips the gate", {
  spy <- new_spy("obs")
  first <- snapshot_session(counter_server(spy))
  rec <- first$.snapshotRegistry$snapshot(); rec$identity <- snapshot_identity(first)
  rec$sources[["|reactiveVal|count"]]$bytes <- as.raw(c(1, 2, 3))
  s2 <- MockShinySession$new()
  srv <- capture_env(counter_server(spy))
  out <- s2$.snapshotRegistry$resume(rec, list(inputs = list(), clientData = list(), fileInputs = character(0)), FALSE,
    function() withMockContext(s2, srv(s2$input, s2$output, s2)))
  expect_identical(out$resumed, "inputs")
  expect_match(out$cause, "saved value could not be applied")
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
