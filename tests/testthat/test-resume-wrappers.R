# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("bindEvent(ignoreInit = TRUE) on an adopted observer and render function handles the next event", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observeEvent(input$go, { spy$obs <- spy$obs + 1 }, ignoreInit = TRUE)
    output$txt <- renderText({ spy$out <- spy$out + 1; input$x }) |> bindEvent(input$go, ignoreInit = TRUE)
  }}
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(make(spy), function(s) { s$setInputs(go = 0, x = "a"); s$setInputs(go = 1); s$getOutput("txt") })
  s2 <- rt$second
  expect_equal(spy$obs, 0); expect_equal(spy$out, 0)
  s2$setInputs(x = "b")
  expect_equal(spy$obs, 0)
  s2$setInputs(go = 2)
  expect_equal(spy$obs, 1)
  expect_equal(s2$getOutput("txt"), "b")
  expect_equal(spy$out, 1)
})

test_that("debounce() still propagates the first change after adoption; throttle() and reactivePoll() are unchanged", {
  make <- function(spy) { force(spy); function(input, output, session) {
    raw <- reactive(input$x)
    slow <- debounce(raw, 500)
    # Lists, so reset_spies() clears them rather than zeroing them.
    observe({ spy$seen <- c(spy$seen, list(slow())) })
    fast <- throttle(raw, 500)
    observe({ spy$fast <- c(spy$fast, list(fast())) })
    polled <- reactivePoll(1000, session, function() { spy$polls <- spy$polls + 1; input$x }, function() input$x)
    observe({ polled() })
  }}
  spy <- new_spy("polls"); spy$seen <- NULL; spy$fast <- NULL
  rt <- snapshot_roundtrip(make(spy), function(s) { s$setInputs(x = 1); s$elapse(600) })
  s2 <- rt$second
  expect_null(spy$seen)
  s2$setInputs(x = 2)
  s2$elapse(600)
  expect_equal(spy$seen, list(2))
  expect_true(2 %in% spy$fast)
  before <- spy$polls
  s2$elapse(1000)
  expect_gt(spy$polls, before)
})

test_that("ExtendedTask: a completed result is adopted, a running one becomes an interrupted error, two tasks stay distinct", {
  make <- function(spy) { force(spy); function(input, output, session) {
    done <- ExtendedTask$new(function(x) { spy$runs <- spy$runs + 1; promises::promise_resolve(x * 2) })
    stuck <- ExtendedTask$new(function() promises::promise(function(resolve, reject) NULL))
    observeEvent(input$go, { done$invoke(input$go); stuck$invoke() })
    output$txt <- renderText(done$result())
  }}
  spy <- new_spy("runs")
  rt <- snapshot_roundtrip(make(spy), function(s) {
    s$setInputs(go = 21)
    for (i in 1:5) { later::run_now(); s$flushReact() }
    expect_equal(s$getOutput("txt"), "42")
    expect_equal(isolate(s$env$stuck$status()), "running")
  })
  s2 <- rt$second
  expect_equal(isolate(s2$env$done$result()), 42)
  expect_equal(spy$runs, 0)
  expect_equal(isolate(s2$env$stuck$status()), "error")
  expect_error(isolate(s2$env$stuck$result()), "interrupted")
  expect_true(any(grepl("reactiveVal:ExtendedTask done#1$", rt$snapshot$report$id)))
  expect_true(any(grepl("reactiveVal:ExtendedTask stuck#1$", rt$snapshot$report$id)))
})

test_that("an unassigned ExtendedTask does not use its `<anonymous>` label as identity", {
  server <- function(input, output, session) {
    tasks <- list(ExtendedTask$new(function() promises::promise_resolve(1)))
  }
  ids <- snapshot_short_ids(snapshot_session(server))
  expect_false(any(grepl("<anonymous>", ids, fixed = TRUE)))
  expect_length(grep("^reactiveVal:", ids), 1)
})

test_that("two unassigned ExtendedTasks get a nudge that says how to fix them", {
  seen <- character(0)
  local_mocked_bindings(
    devmode_inform = function(message, ...) seen <<- c(seen, message),
    get_call_srcref = function(...) NULL   # an installed package keeps no srcrefs
  )
  server <- function(input, output, session) {
    tasks <- list(
      ExtendedTask$new(function() promises::promise_resolve(1)),
      ExtendedTask$new(function() promises::promise_resolve(2))
    )
  }
  snapshot_session(server)
  expect_length(seen, 1)
  expect_match(seen, "assign each ExtendedTask to a distinct name", fixed = TRUE)
})

test_that("ExtendedTask status(), result() and invoke() behave as before on a fresh session", {
  server <- function(input, output, session) {
    t <- ExtendedTask$new(function(x) if (x < 0) promises::promise_reject(simpleError("neg")) else promises::promise_resolve(x))
  }
  s <- snapshot_session(server)
  t <- s$env$t
  expect_identical(isolate(t$status()), "initial")
  expect_error(isolate(t$result()), class = "shiny.silent.error")
  t$invoke(1); for (i in 1:3) { later::run_now(); s$flushReact() }
  expect_identical(isolate(t$status()), "success"); expect_identical(isolate(t$result()), 1)
  t$invoke(-1)
  # The warning is raised by the promise rejection handler, on the event loop.
  expect_warning(for (i in 1:3) { later::run_now(); s$flushReact() }, "ExtendedTask")
  expect_identical(isolate(t$status()), "error"); expect_error(isolate(t$result()), "neg")
})

test_that("bindCache() on renderPlot creates its resize observer eagerly, so the output is not a constructor observer", {
  server <- function(input, output, session) {
    output$plot <- renderPlot(plot(1)) |> bindCache(input$n)
  }
  s <- snapshot_session(server)
  s$setInputs(n = 1)
  s$getOutput("plot")
  rep <- s$resumeReport()
  expect_true(rep$adoptable[grepl("output:output\\$plot#1$", rep$id)])
  expect_true(any(grepl("\\|top\\|observer:plot-resize#1$", rep$id)))
  expect_true(any(grepl("reactiveVal:bindCache renderPlot fitDims#1$", rep$id)))
})

test_that("a cached renderPlot built outside a session creates its resize observer at first render", {
  plot_fn <- renderPlot(plot(1)) |> bindCache(1)
  server <- function(input, output, session) {
    output$plot <- plot_fn
  }
  s <- snapshot_session(server)
  expect_false(any(grepl("observer:plot-resize#", s$.snapshotRegistry$ids())))
  s$getOutput("plot")
  expect_true(any(grepl("observer:plot-resize#", s$.snapshotRegistry$ids())))
})

test_that("Shiny's internal sources are labelled, so two cached renderPlots in a scope raise no ambiguity nudge", {
  # An installed package has no srcrefs, for its own code or the app's.
  local_mocked_bindings(get_call_srcref = function(...) NULL)
  code <- 'server <- function(input, output, session) {
    output$a <- renderPlot(plot(1)) |> bindCache(input$n)
    output$b <- renderPlot(plot(2)) |> bindCache(input$n)
  }'
  env <- snapshot_eval_no_srcref(code, new.env())
  s <- snapshot_session(env$server)
  informed <- s$.snapshotRegistry$.__enclos_env__$private$informed
  expect_false(any(startsWith(informed, "ambiguous-source:")))
})

test_that("a downloadHandler() output re-runs after adoption, so its URL carries the new session", {
  server <- function(input, output, session) {
    output$dl <- downloadHandler("x.txt", function(file) writeLines("hi", file))
  }
  rt <- snapshot_roundtrip(server)
  row <- rt$report[rt$report$label == "output$dl", ]
  expect_false(row$adoptable)
  expect_match(row$reason, "uses a session URL")
  expect_length(rt$outcome$adopted, 0)
  expect_true(rt$second$.__enclos_env__$private$file_generators$has("mock-session-dl"))
})

test_that("a cached renderPlot tracks its own output name after adoption, even when plots were reordered", {
  ab <- function(input, output, session) {
    output$a <- renderPlot(plot(1)) |> bindCache(input$n)
    output$b <- renderPlot(plot(2)) |> bindCache(input$n)
  }
  ba <- function(input, output, session) {
    output$b <- renderPlot(plot(2)) |> bindCache(input$n)
    output$a <- renderPlot(plot(1)) |> bindCache(input$n)
  }
  first <- snapshot_session(ab)
  first$setInputs(n = 1); first$getOutput("a"); first$getOutput("b")
  snap <- first$.snapshotRegistry$snapshot()
  s2 <- adopt_snapshot_into(snap, ba)
  s2$setInputs(n = 2); s2$getOutput("a"); s2$getOutput("b")
  reg <- s2$.snapshotRegistry
  ids <- grep("reactiveVal:bindCache renderPlot outputName#", reg$ids(), value = TRUE)
  names <- vapply(ids, function(id) isolate(rlang::wref_key(reg$nodeEntry(id)$node)$.snapshot$value()), character(1))
  # Definition order in the adopting session: b, then a.
  expect_identical(unname(names), c("b", "a"))
})

test_that("one cached renderPlot shared by two outputs of different sizes settles", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  ws <- list(request = list(), send = function(msg) NULL, close = function() NULL)
  s <- ShinySession$new(ws)
  calls <- 0
  # Fails the flush rather than hanging the suite if the outputs keep
  # invalidating each other.
  policy <- function(dims) {
    calls <<- calls + 1
    if (calls > 50) stop("sizePolicy called more than 50 times")
    dims
  }
  withReactiveDomain(s, isolate({
    p <- bindCache(renderPlot(plot(1)), 1, cache = cachem::cache_mem(), sizePolicy = policy)
    s$output$a <- p
    s$output$b <- p
  }))
  s$manageInputs(list(
    .clientdata_output_a_width = 400, .clientdata_output_a_height = 400,
    .clientdata_output_b_width = 900, .clientdata_output_b_height = 400,
    .clientdata_output_a_hidden = FALSE, .clientdata_output_b_hidden = FALSE,
    .clientdata_pixelratio = 1
  ), now = TRUE)
  expect_no_error(for (i in 1:3) { flushReact(); s$flushOutput() })
  expect_lte(calls, 4)
})
