# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

counter_server <- function(spy) {
  force(spy)
  function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, { spy$obs <- spy$obs + 1; count(count() + 1) })
    output$txt <- renderText({ spy$out <- spy$out + 1; paste("Count:", count()) })
  }
}

test_that("a counter is adopted at 3, the observer is not re-fired, the next click gives 4", {
  spy <- new_spy("obs", "out")
  rt <- snapshot_roundtrip(counter_server(spy), function(s) { for (i in 1:3) s$setInputs(plus = i) })
  expect_equal(isolate(rt$first$env$count()), 3)
  s2 <- rt$second
  expect_equal(isolate(s2$env$count()), 3)
  expect_equal(spy$obs, 0)
  expect_equal(spy$out, 0)
  s2$setInputs(plus = 4)
  expect_equal(isolate(s2$env$count()), 4)
  expect_equal(spy$obs, 1)
  expect_equal(s2$getOutput("txt"), "Count: 4")
  expect_true(all(rt$report$adoptable[rt$report$kind %in% c("observer", "output", "reactiveVal")]))
})

test_that("a side-effect observer is not re-fired; a constructor observer re-runs and its children adopt", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observe({ input$x; spy$emails <- spy$emails + 1 })
    mods <- reactiveValues()
    observeEvent(input$add, {
      spy$adds <- spy$adds + 1
      mods[[input$add]] <- moduleServer(input$add, function(input, output, session) {
        v <- reactiveVal(0)
        observeEvent(input$plus, v(v() + 1))
        v
      })
    })
  }}
  drive <- function(s) { s$setInputs(x = 1); s$setInputs(add = "m1"); s$setInputs(`m1-plus` = 1); s$setInputs(`m1-plus` = 2) }
  spy <- new_spy("emails", "adds")
  rt <- snapshot_roundtrip(make(spy), drive)
  expect_equal(spy$emails, 0)
  expect_equal(spy$adds, 1)
  expect_equal(isolate(rt$second$env$mods$m1()), 2)
  expect_match(rt$outcome$fallbacks$reason[grepl("observeEvent|observer:", rt$outcome$fallbacks$id)][1], "creates other reactives")
})

test_that("a conductor computes on the first downstream read only, once", {
  make <- function(spy) { force(spy); function(input, output, session) {
    expensive <- reactive({ spy$runs <- spy$runs + 1; input$n * 100 })
    output$txt <- renderText(expensive())
  }}
  spy <- new_spy("runs")
  rt <- snapshot_roundtrip(make(spy), function(s) s$setInputs(n = 2))
  expect_equal(spy$runs, 0)
  expect_equal(isolate(rt$second$env$expensive()), 200)
  expect_equal(spy$runs, 1)
  rt$second$setInputs(n = 3)
  expect_equal(rt$second$getOutput("txt"), "300")
  expect_equal(spy$runs, 2)
})

test_that("sentinel edges: a source two conductors upstream invalidates the adopted observer; an unrelated one does not", {
  make <- function(spy) { force(spy); function(input, output, session) {
    a <- reactiveVal(1); b <- reactiveVal(1)
    r1 <- reactive(a() * 2); r2 <- reactive(r1() + 1)
    o <- observe({ r2(); spy$o <- spy$o + 1 })
  }}
  spy <- new_spy("o")
  rt <- snapshot_roundtrip(make(spy))
  s2 <- rt$second
  expect_equal(spy$o, 0)
  isolate(s2$env$b(2)); s2$flushReact()
  expect_equal(spy$o, 0)
  isolate(s2$env$a(2)); s2$flushReact()
  expect_equal(spy$o, 1)
  # After running, real edges replaced the sentinels; a() still reaches it.
  isolate(s2$env$a(3)); s2$flushReact()
  expect_equal(spy$o, 2)
})

test_that("consistency invariant: an excluded source forces its transitive dependents to run", {
  make <- function(spy) { force(spy); function(input, output, session) {
    handle <- reactiveVal(new.env())
    fine <- reactiveVal(1)
    r <- reactive({ handle(); 1 })
    dep <- observe({ r(); spy$dep <- spy$dep + 1 })
    other <- observe({ fine(); spy$other <- spy$other + 1 })
  }}
  spy <- new_spy("dep", "other")
  rt <- snapshot_roundtrip(make(spy))
  expect_equal(spy$dep, 1)
  expect_equal(spy$other, 0)
  expect_identical(rt$outcome$fallbacks$reason[grepl("observer:dep#1$", rt$outcome$fallbacks$id)], "depends on a source that was not restored")
})

test_that("positional identity: a hash-labelled group whose sibling count changed is reverted, never misadopted", {
  code <- 'make <- function(spy, skip_a) { force(spy); force(skip_a); function(input, output, session) {
    if (!skip_a) a <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    observe({ spy$seen <- b() })
  }}'
  env <- snapshot_eval_no_srcref(code, new.env())
  spy1 <- new_spy(); spy2 <- new_spy()
  first <- snapshot_session(env$make(spy1, FALSE))
  isolate({ first$env$a("A"); first$env$b("B") })
  first$flushReact()
  snap <- first$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(first)
  s2 <- adopt_snapshot_into(snap, env$make(spy2, TRUE))
  expect_null(isolate(s2$env$b()))                       # not "A"
  expect_null(spy2$seen)
  fb <- s2$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_match(fb$reason[grepl("reactiveVal:", fb$id)], "positional identity: sibling count differs \\(snapshot 2, now 1\\)")
  expect_identical(fb$reason[grepl("observer:", fb$id)], "depends on a source that was not restored")
  # The same code with the same count adopts by position as before.
  s3 <- adopt_snapshot_into(snap, env$make(new_spy(), FALSE))
  expect_identical(isolate(s3$env$a()), "A")
  expect_identical(isolate(s3$env$b()), "B")
})

test_that("positional identity: an initial value of a different shape is not adopted; a labelled source still is", {
  code <- 'make <- function(init) { force(init); function(input, output, session) {
    x <- reactiveVal(init)
    y <- reactiveVal(init)
  }}'
  env <- snapshot_eval_no_srcref(code, new.env())
  first <- snapshot_session(env$make(list()))
  isolate(first$env$x(list(1)))
  snap <- first$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(first)
  s2 <- adopt_snapshot_into(snap, env$make(NULL))
  expect_null(isolate(s2$env$x()))
  fb <- s2$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_length(fb$id, 2)
  expect_true(all(fb$reason == "positional identity: initial value shape differs"))
  # Saved state beats a changed initial value when identity is not positional.
  labelled <- function(input, output, session) { count <- reactiveVal(0) }
  f <- snapshot_session(labelled); isolate(f$env$count(3))
  snap2 <- f$.snapshotRegistry$snapshot(); snap2$identity <- snapshot_identity(f)
  s3 <- adopt_snapshot_into(snap2, function(input, output, session) { count <- reactiveVal("zero") })
  expect_identical(isolate(s3$env$count()), 3)
  expect_length(s3$.snapshotRegistry$adoptOutcome()$fallbacks$id, 0)
})

test_that("body-hash and path-hash mismatches each force a run", {
  first <- function(input, output, session) {
    r <- reactive(input$x + 1)
    o <- observe({ r(); message("A") })
  }
  s1 <- snapshot_session(first)
  expect_message(s1$setInputs(x = 1), "A")
  snap <- s1$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(s1)
  adopt_into <- function(server) {
    s2 <- adopt_snapshot_into(snap, server, flush = FALSE)
    expect_message(s2$flushReact(), "A")
    s2$.snapshotRegistry$adoptOutcome()
  }
  body_changed <- function(input, output, session) {
    r <- reactive(input$x + 1)
    o <- observe({ r(); message("A"); NULL })
  }
  path_changed <- function(input, output, session) {
    r <- reactive(input$x + 2)
    o <- observe({ r(); message("A") })
  }
  expect_identical(adopt_into(body_changed)$fallbacks$reason, "body changed")
  expect_identical(adopt_into(path_changed)$fallbacks$reason, "a reactive() on its dependency path changed")
})

test_that("destroyed observers are destroyed on adopt; suspended ones are adopted suspended with a pending run surviving", {
  make <- function(spy) { force(spy); function(input, output, session) {
    once <- observeEvent(input$go, { spy$once <- spy$once + 1 }, once = TRUE)
    self_destroy <- observe({ spy$sd <- spy$sd + 1; self_destroy$destroy() })
    paused <- observe({ input$p; spy$paused <- spy$paused + 1 }, suspended = TRUE)
  }}
  spy <- new_spy("once", "sd", "paused")
  rt <- snapshot_roundtrip(make(spy), function(s) {
    s$setInputs(go = 1)
    s$env$paused$resume(); s$flushReact()   # ran once
    s$env$paused$suspend()
    s$setInputs(p = 2)                        # invalidated while suspended: run pending
  })
  s2 <- rt$second
  expect_equal(spy$once, 0); expect_equal(spy$sd, 0); expect_equal(spy$paused, 0)
  s2$setInputs(go = 2)
  expect_equal(spy$once, 0)
  expect_true(s2$env$paused$.suspended)
  s2$env$paused$resume(); s2$flushReact()
  expect_equal(spy$paused, 1)
})

test_that("only dependents of inputs that changed while disconnected run; snapshot-only keys keep their value; new keys apply", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observe({ input$a; spy$a <- spy$a + 1 })
    observe({ input$b; spy$b <- spy$b + 1 })
    observe({ input$gone; spy$gone <- spy$gone + 1 })
  }}
  spy <- new_spy("a", "b", "gone")
  rt <- snapshot_roundtrip(make(spy), function(s) s$setInputs(a = 1, b = 1, gone = 1),
                           live = list(a = 1, b = 2, new = 9))
  expect_equal(spy$a, 0)
  expect_equal(spy$b, 1)
  expect_equal(spy$gone, 0)
  expect_identical(isolate(rt$second$input$gone), 1)
  expect_identical(isolate(rt$second$input$new), 9)
})

test_that("a fileInput's adopted value survives the client's NULL re-send while its upload exists, and is dropped once it is gone", {
  make <- function(spy) { force(spy); function(input, output, session) { observe({ spy$runs <- spy$runs + 1; spy$seen <- input$upload }) } }
  up <- withr::local_tempdir()
  path <- file.path(up, "a.csv"); writeLines("x", path)
  df <- data.frame(name = "a.csv", size = 1, type = "text/csv", datapath = path, stringsAsFactors = FALSE)
  s1 <- snapshot_session(make(new_spy("runs")))
  s1$setInputs(upload = df)
  .subset2(s1$input, "impl")$setMeta("upload", "shiny.serializer", serializerFileInput)
  snap <- s1$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(s1)
  expect_identical(snap$fileInputs, "upload")

  adopt_file <- function(spy) {
    s2 <- MockShinySession$new()
    live <- snapshot_live_inputs(list(`upload:shiny.file` = NULL))
    expect_identical(live$fileInputs, "upload")
    live <- snapshot_apply_handlers(live, s2)
    expect_identical(names(live$inputs), "upload")
    adopt_snapshot_into(snap, make(spy), s2, liveSet = live)
  }
  spy <- new_spy("runs")
  s2 <- adopt_file(spy)
  expect_identical(isolate(s2$input$upload), df)
  expect_equal(spy$runs, 0)

  unlink(path)                                      # the previous process's tempdir is gone
  spy3 <- new_spy("runs")
  s3 <- adopt_file(spy3)
  expect_null(isolate(s3$input$upload))
  expect_equal(spy3$runs, 1)                        # dependents run, as on a fresh start
  fb <- s3$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_identical(fb$reason[fb$id == "input$upload"], "upload directory no longer exists")
})

test_that("an observer that reads an input never set in either session is adopted, not re-run", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observe({ input$optional; spy$opt <- spy$opt + 1 })
    observe({ input$arrived; spy$arr <- spy$arr + 1 })
  }}
  spy <- new_spy("opt", "arr")
  rt <- snapshot_roundtrip(make(spy), live = list(arrived = 1))
  expect_equal(spy$opt, 0)
  expect_equal(spy$arr, 1)
  expect_identical(isolate(rt$second$input$arrived), 1)
})

test_that("userData, module state under two namespaces, and reactiveValues keys are restored", {
  make <- function() function(input, output, session) {
    session$userData$note <- "x"
    rv <- reactiveValues(n = 0, log = character())
    observeEvent(input$plus, { rv$n <- rv$n + 1; rv$log <- c(rv$log, "click") })
    m <- function(id) moduleServer(id, function(input, output, session) {
      v <- reactiveVal(0); observeEvent(input$plus, v(v() + 1)); v
    })
    a <- m("a"); b <- m("b")
  }
  rt <- snapshot_roundtrip(make(), function(s) {
    s$userData$note <- "changed"
    s$setInputs(plus = 1, `a-plus` = 1); s$setInputs(`a-plus` = 2); s$setInputs(`b-plus` = 1)
  })
  s2 <- rt$second
  expect_identical(s2$userData$note, "changed")
  expect_identical(isolate(s2$env$rv$n), 1)
  expect_identical(isolate(s2$env$rv$log), "click")
  expect_equal(isolate(s2$env$a()), 2)
  expect_equal(isolate(s2$env$b()), 1)
})

test_that("onResumed() fires once after commit and before the first flush; a fallback observer sees its side state", {
  make <- function(spy) { force(spy); function(input, output, session) {
    conn <- NULL
    session$onResumed(function(info) { spy$info <- info; spy$fired <- spy$fired + 1; conn <<- "rebuilt" })
    observe({ invalidateLater(1e6); spy$seen <- conn })   # never adopted: timer
  }}
  spy <- new_spy("fired")
  rt <- snapshot_roundtrip(make(spy))
  expect_equal(spy$fired, 1)
  expect_identical(spy$info, list(from = "reconnect", resumed = "snapshot", dom = "intact"))
  expect_identical(spy$seen, "rebuilt")
  fb <- rt$outcome$fallbacks
  expect_match(fb$reason[grepl("observer:", fb$id)], "timer")      # the recorded reason, not "not in the snapshot"
  # Never on a plain start:
  spy2 <- new_spy("fired")
  s <- snapshot_session(make(spy2)); s$flushReact()
  expect_equal(spy2$fired, 0)
})

test_that("a password input is absent from the snapshot, reported, and present again from live inputs; restore stays inactive", {
  make <- function(spy) { force(spy); function(input, output, session) {
    observe({ input$pw; spy$pw <- spy$pw + 1 })
  }}
  spy <- new_spy("pw")
  drive <- function(s) { s$setInputs(pw = "secret"); .subset2(s$input, "impl")$setMeta("pw", "shiny.serializer", serializerUnserializable) }
  rt <- snapshot_roundtrip(make(spy), drive, live = list(pw = "secret"))
  expect_null(rt$snapshot$inputs$pw)
  expect_true("input$pw" %in% rt$snapshot$report$id)
  expect_identical(isolate(rt$second$input$pw), "secret")
  expect_false(rt$second$restoreContext$active)
  expect_gte(spy$pw, 1)
})

test_that("hash parts: a changed event expression is not adopted; a flipped ignoreInit counts as a change", {
  base <- function(input, output, session) { o <- observeEvent(input$a, { message("run") }) }
  s1 <- snapshot_session(base)
  expect_message(s1$setInputs(a = 1), "run")
  snap <- s1$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(s1)
  outcome <- function(server) {
    s2 <- adopt_snapshot_into(snap, server, flush = FALSE)
    suppressMessages({ s2$flushReact(); s2$flushReact() })
    s2$.snapshotRegistry$adoptOutcome()
  }
  expect_length(outcome(base)$fallbacks$id, 0)
  event_changed <- function(input, output, session) { o <- observeEvent(input$b, { message("run") }) }
  init_flipped <- function(input, output, session) { o <- observeEvent(input$a, { message("run") }, ignoreInit = TRUE) }
  expect_identical(outcome(event_changed)$fallbacks$reason, "body changed")
  expect_identical(outcome(init_flipped)$fallbacks$reason, "body changed")
})

test_that("hash parts: a render function's bindEvent() events and flags, and its bindCache() key, count as a change", {
  base <- function(input, output, session) {
    output$txt <- renderText(input$x) |> bindCache(input$x) |> bindEvent(input$a)
  }
  s1 <- snapshot_session(base)
  s1$setInputs(a = 1, b = 1, x = "v", y = "w")
  s1$getOutput("txt")
  snap <- s1$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(s1)
  fallbacks <- function(server) adopt_snapshot_into(snap, server)$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_length(fallbacks(base)$id, 0)
  changed <- list(
    event = function(input, output, session) {
      output$txt <- renderText(input$x) |> bindCache(input$x) |> bindEvent(input$b)
    },
    ignoreInit = function(input, output, session) {
      output$txt <- renderText(input$x) |> bindCache(input$x) |> bindEvent(input$a, ignoreInit = TRUE)
    },
    ignoreNULL = function(input, output, session) {
      output$txt <- renderText(input$x) |> bindCache(input$x) |> bindEvent(input$a, ignoreNULL = FALSE)
    },
    cacheKey = function(input, output, session) {
      output$txt <- renderText(input$x) |> bindCache(input$y) |> bindEvent(input$a)
    }
  )
  for (nm in names(changed)) {
    fb <- fallbacks(changed[[nm]])
    expect_match(fb$id, "output:output$txt#1", fixed = TRUE, info = nm)
    expect_identical(fb$reason, "body changed", info = nm)
  }
})

test_that("path hashes: a bound reactive()'s events, flags, cache key and body count as a change", {
  conductor_app <- function(make_r) {
    function(input, output, session) {
      r <- make_r(input)
      o <- observe(r())
    }
  }
  apps <- list(
    bindEvent = list(
      base = function(input) reactive(input$x * 2) |> bindEvent(input$a),
      event = function(input) reactive(input$x * 2) |> bindEvent(input$b),
      ignoreInit = function(input) reactive(input$x * 2) |> bindEvent(input$a, ignoreInit = TRUE),
      ignoreNULL = function(input) reactive(input$x * 2) |> bindEvent(input$a, ignoreNULL = FALSE),
      body = function(input) reactive(input$x * 3) |> bindEvent(input$a)
    ),
    eventReactive = list(
      base = function(input) eventReactive(input$a, input$x * 2),
      event = function(input) eventReactive(input$b, input$x * 2),
      ignoreInit = function(input) eventReactive(input$a, input$x * 2, ignoreInit = TRUE),
      body = function(input) eventReactive(input$a, input$x * 3)
    ),
    bindCache = list(
      base = function(input) reactive(input$x * 2) |> bindCache(input$x),
      cacheKey = function(input) reactive(input$x * 2) |> bindCache(input$y),
      body = function(input) reactive(input$x * 3) |> bindCache(input$x)
    )
  )
  for (kind in names(apps)) {
    variants <- apps[[kind]]
    s1 <- snapshot_session(conductor_app(variants$base))
    s1$setInputs(a = 1, b = 1, x = 1, y = 1)
    snap <- s1$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(s1)
    fallbacks <- function(make_r) {
      adopt_snapshot_into(snap, conductor_app(make_r))$.snapshotRegistry$adoptOutcome()$fallbacks
    }
    expect_length(fallbacks(variants$base)$id, 0)
    for (nm in setdiff(names(variants), "base")) {
      fb <- fallbacks(variants[[nm]])
      info <- paste(kind, nm)
      expect_match(fb$id, "observer:o#1", fixed = TRUE, info = info)
      expect_identical(fb$reason, "a reactive() on its dependency path changed", info = info)
    }
  }
})

test_that("inputs-only resume seeds RestoreContext inactive: restoreInput() sees the value", {
  server <- capture_env(function(input, output, session) {
    output$ui <- renderUI(textInput("t", "t", value = restoreInput("t", "default")))
  })
  s <- MockShinySession$new()
  run <- function() withMockContext(s, server(input = s$input, output = s$output, session = s))
  info <- s$.snapshotRegistry$resumeFromInputs(list(inputs = list(t = "typed"), clientData = list(), fileInputs = character(0)), run,
                                               list(from = "reconnect", resumed = "inputs", dom = "intact"))
  s$flushReact()
  expect_identical(info$resumed, "inputs")
  expect_false(s$restoreContext$active)
  expect_match(as.character(s$getOutput("ui")$html), 'value="typed"')
})

test_that("disableResume() on a render function re-renders that output only; on an observer it runs with ignoreInit honoured", {
  make <- function(spy) { force(spy); function(input, output, session) {
    output$clock <- renderText({ spy$clock <- spy$clock + 1; "t" }) |> disableResume()
    output$plain <- renderText({ spy$plain <- spy$plain + 1; "p" })
    observeEvent(input$go, { spy$skipped <- spy$skipped + 1 }, ignoreInit = TRUE) |> disableResume()
  }}
  spy <- new_spy("clock", "plain", "skipped")
  rt <- snapshot_roundtrip(make(spy), function(s) s$setInputs(go = 1))
  expect_equal(spy$clock, 1)
  expect_equal(spy$plain, 0)
  expect_equal(spy$skipped, 0)
  rt$second$setInputs(go = 2)
  expect_equal(spy$skipped, 1)
})

test_that("a reactiveVal created inside a reactive() is not snapshotted and its dependents run", {
  make <- function(spy) { force(spy); function(input, output, session) {
    # Held by the server function: the registry holds nodes weakly, and a
    # collected reactiveVal leaves neither a report row nor an edge.
    inner <- NULL
    r <- reactive({ inner <<- reactiveVal(1); inner() })
    o <- observe({ r(); spy$o <- spy$o + 1 })
  }}
  spy <- new_spy("o")
  rt <- snapshot_roundtrip(make(spy))
  expect_equal(spy$o, 1)
  expect_identical(rt$snapshot$report$reason[grepl("reactiveVal:inner#1$", rt$snapshot$report$id)], "created inside a reactive()")
})

test_that("a not-up-to-date adopted observer runs once at first flush", {
  make <- function(spy) { force(spy); function(input, output, session) {
    o <- observe({ input$x; spy$o <- spy$o + 1 })
  }}
  spy <- new_spy("o")
  # Snapshot taken with an invalidation pending: setInputs flushes, so
  # invalidate the context by hand after driving. The first session's
  # pending run also happens at the next (global) flush, so it gets its own
  # spy.
  first <- snapshot_session(make(new_spy("o"))); first$setInputs(x = 1)
  isolate(.subset2(first$input, "impl")$set("x", 2)) # no flush
  snap <- first$.snapshotRegistry$snapshot(); snap$identity <- snapshot_identity(first)
  s2 <- adopt_snapshot_into(snap, make(spy))
  expect_equal(spy$o, 1)
  expect_true("observer:o#1" %in% sub("^[^|]*\\|[^|]*\\|", "", s2$.snapshotRegistry$adoptOutcome()$adopted))
})

test_that("deciding one node throwing is a per-node mismatch; the others adopt", {
  make <- function(spy) { force(spy); function(input, output, session) {
    o1 <- observe({ input$x; spy$o1 <- spy$o1 + 1 })
    o2 <- observe({ input$x; spy$o2 <- spy$o2 + 1 })
  }}
  spy <- new_spy("o1", "o2")
  first <- snapshot_session(make(spy)); first$setInputs(x = 1)
  snap <- first$.snapshotRegistry$snapshot()
  o1 <- grep("observer:o1#1$", names(snap$nodes), value = TRUE)
  snap$nodes[[o1]]$pathHashes <- list(1)   # hasConductorHashes() throws on it
  s2 <- adopt_snapshot_into(snap, make(spy))
  expect_equal(c(spy$o1, spy$o2), c(1, 0))
  fb <- s2$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_match(fb$reason[fb$id == o1], "^adoption failed: ")
})

# Hash-labelled sources (no srcrefs) whose record carries an uncomparable
# sibling count: verifySiblingCounts() then throws inside commit itself,
# outside any per-node handler.
corrupt_siblings <- function(snap, pattern) {
  id <- grep(pattern, names(snap$nodes), value = TRUE)
  stopifnot(length(id) == 1)
  snap$nodes[[id]]$siblings <- sum
  snap
}

test_that("if the first commit throws, adoption is abandoned and every observer runs", {
  code <- 'make <- function(spy) { force(spy); function(input, output, session) {
    x <- reactiveVal(1)
    observe({ input$x; spy$o1 <- spy$o1 + 1 })
    observe({ x(); spy$o2 <- spy$o2 + 1 })
  }}'
  env <- snapshot_eval_no_srcref(code, new.env())
  spy <- new_spy("o1", "o2")
  first <- snapshot_session(env$make(spy)); first$setInputs(x = 1)
  snap <- corrupt_siblings(first$.snapshotRegistry$snapshot(), "reactiveVal:")
  s2 <- adopt_snapshot_into(snap, env$make(spy))
  expect_equal(c(spy$o1, spy$o2), c(1, 1))
  outcome <- s2$.snapshotRegistry$adoptOutcome()
  expect_length(outcome$adopted, 0)
  obs <- outcome$fallbacks[outcome$fallbacks$kind == "observer", ]
  expect_equal(nrow(obs), 2)
  expect_match(obs$reason, "^commit failed: ")
})

test_that("if the second commit throws, first-commit adoptees run too and nothing later adopts", {
  code <- 'make <- function(spy) { force(spy); function(input, output, session) {
    holder <- NULL
    observe({ input$x; spy$top <- spy$top + 1 })
    observeEvent(input$add, {
      spy$ctor <- spy$ctor + 1
      holder <<- reactiveVal(0)
      observe({ holder(); spy$inner <- spy$inner + 1 })
    })
  }}'
  env <- snapshot_eval_no_srcref(code, new.env())
  spy <- new_spy("top", "ctor", "inner")
  first <- snapshot_session(env$make(spy)); first$setInputs(x = 1, add = 1)
  snap <- corrupt_siblings(first$.snapshotRegistry$snapshot(), "reactiveVal:")
  s2 <- adopt_snapshot_into(snap, env$make(spy), flush = FALSE)
  s2$flushReact()
  # The first commit adopted the top-level observer; the constructor re-ran.
  expect_equal(c(spy$top, spy$ctor, spy$inner), c(0, 1, 0))
  s2$flushReact()
  expect_equal(c(spy$top, spy$ctor, spy$inner), c(1, 1, 1))
  outcome <- s2$.snapshotRegistry$adoptOutcome()
  expect_length(outcome$adopted, 0)
  fb <- outcome$fallbacks
  expect_equal(sum(grepl("^commit failed: ", fb$reason[fb$kind == "observer"])), 2)
})

test_that("if the server function throws mid-adopt, adopt mode ends and the session resumes from inputs", {
  spy <- new_spy("before", "after")
  snap <- snapshot_session(counter_server(new_spy("obs", "out")))$.snapshotRegistry$snapshot()
  failing <- function(input, output, session) {
    observe(spy$before <- spy$before + 1)
    stop("boom")
  }
  s <- MockShinySession$new()
  expect_error(adopt_snapshot_into(snap, failing, s, flush = FALSE), "boom")
  expect_false(s$.snapshotRegistry$inAdoptMode())
  s$flushReact()
  expect_equal(spy$before, 1)    # the observer built before the error was released
  ok <- function(input, output, session) observe(spy$after <- spy$after + 1)
  run <- function() withMockContext(s, ok(input = s$input, output = s$output, session = s))
  s$.snapshotRegistry$resumeFromInputs(list(inputs = list(), clientData = list(), fileInputs = character(0)), run,
                                       list(from = "reconnect", resumed = "inputs", dom = "intact"))
  s$flushReact()
  expect_equal(spy$after, 1)
})

test_that("reverting a source invalidates whoever read it", {
  s <- MockShinySession$new()
  seen <- new.env()
  withMockContext(s, {
    v <- reactiveVal("adopted", label = "v")
    rv <- reactiveValues(k = "adopted")
    observe(seen$v <- v())
    observe(seen$k <- rv$k)
  })
  s$flushReact()
  attr(v, ".impl")$.snapshot$revert("initial")
  .subset2(rv, "impl")$.snapshot$revert(list(k = "initial"))
  s$flushReact()
  expect_identical(seen$v, "initial")
  expect_identical(seen$k, "initial")
})

test_that("the resume log line names counts and up to five fallbacks", {
  outcome <- list(
    adopted = paste0("id", 1:38),
    fallbacks = data.frame(
      id = c(paste0("ns|top|observer:o", 1:7, "#1"), "ns|top|reactiveVal:x#1"),
      kind = c(rep("observer", 7), "reactiveVal"),
      reason = "body changed", stringsAsFactors = FALSE
    ),
    hazards = c("id1", "id2")
  )
  lines <- snapshot_resume_log(outcome, list(from = "reconnect", resumed = "snapshot", dom = "intact"))
  expect_identical(lines[[1]], "Resumed (reconnect): restored 38 of 45 reactive nodes, 7 re-ran")
  expect_length(grep("^  observer:o[0-9]#1: body changed$", lines), 5)
  expect_true(any(lines == "  and 2 more; see session$resumeReport()"))
  expect_true(any(lines == "2 adopted observers assign to closure variables; see the hazard column of session$resumeReport()"))
  expect_identical(
    snapshot_resume_log(NULL, list(from = "reconnect", resumed = "inputs", dom = "intact"), cause = "user differs"),
    "Resumed (reconnect) from inputs only: user differs"
  )
})

test_that("the resume log line names where the resume came from", {
  outcome <- list(adopted = "id1", fallbacks = NULL, hazards = character(0))
  expect_identical(
    snapshot_resume_log(outcome, list(from = "reload", resumed = "snapshot", dom = "fresh"))[[1]],
    "Resumed (reload): restored 1 of 1 reactive nodes, 0 re-ran"
  )
  expect_identical(
    snapshot_resume_log(NULL, list(from = "share", resumed = "inputs", dom = "fresh"), cause = "no snapshot"),
    "Resumed (share) from inputs only: no snapshot"
  )
})

test_that("the resume log counts a real adopt's observers and outputs, not its sources", {
  code <- 'make <- function(init) { force(init); function(input, output, session) {
    x <- reactiveVal(init)
    observe({ x(); NULL })
    observe({ input$a; NULL })
    output$txt <- renderText({ input$a; "t" })
  }}'
  env <- snapshot_eval_no_srcref(code, new.env())
  first <- snapshot_session(env$make(list())); first$setInputs(a = 1)
  drain_later()
  snap <- first$.snapshotRegistry$snapshot()
  outcome <- adopt_snapshot_into(snap, env$make(NULL))$.snapshotRegistry$adoptOutcome()
  expect_setequal(outcome$fallbacks$kind, c("reactiveVal", "observer"))
  lines <- snapshot_resume_log(outcome, list(from = "reconnect", resumed = "snapshot", dom = "intact"))
  expect_identical(lines[[1]], "Resumed (reconnect): restored 2 of 3 reactive nodes, 1 re-ran")
  expect_match(lines[[2]], "^  observer:.*: depends on a source that was not restored$")
  expect_length(lines, 2)
})

test_that("a source record that cannot be applied keeps the initial value and its dependents run", {
  make <- function(spy) { force(spy); function(input, output, session) {
    v <- reactiveVal(0)
    rv <- reactiveValues(a = 0)
    observe({ v(); spy$v <- spy$v + 1 })
    observe({ rv$a; spy$rv <- spy$rv + 1 })
  }}
  spy <- new_spy("v", "rv")
  first <- snapshot_session(make(spy))
  isolate({ first$env$v(5); first$env$rv$a <- 5 })
  first$flushReact()
  snap <- first$.snapshotRegistry$snapshot()
  ids <- grep("reactiveVal:v#1$|reactiveValues:rv#1$", names(snap$nodes), value = TRUE)
  expect_length(ids, 2)
  for (id in ids) snap$nodes[[id]]$bytes <- as.raw(1:5)
  s2 <- adopt_snapshot_into(snap, make(spy))
  expect_equal(isolate(s2$env$v()), 0)
  expect_equal(isolate(s2$env$rv$a), 0)
  expect_equal(c(spy$v, spy$rv), c(1, 1))
  fb <- s2$.snapshotRegistry$adoptOutcome()$fallbacks
  expect_match(fb$reason[fb$id %in% ids], "^adoption failed: ")
  expect_length(fb$reason[fb$id %in% ids], 2)
})

test_that("disableResume() on a render function survives a later or earlier bindEvent() or bindCache()", {
  server <- function(input, output, session) {
    output$e1 <- renderText(input$go) |> disableResume() |> bindEvent(input$go)
    output$e2 <- renderText(input$go) |> bindEvent(input$go) |> disableResume()
    output$c1 <- renderText(input$go) |> disableResume() |> bindCache(input$go)
    output$c2 <- renderText(input$go) |> bindCache(input$go) |> disableResume()
  }
  rt <- snapshot_roundtrip(server, function(s) s$setInputs(go = 1))
  outs <- paste0("output$", c("e1", "e2", "c1", "c2"))
  rows <- rt$report[match(outs, rt$report$label), ]
  expect_identical(rows$reason, rep("disableResume()", 4))
  fb <- rt$outcome$fallbacks
  expect_setequal(fb$reason[grepl("output:", fb$id)], "disableResume()")
  expect_length(rt$outcome$adopted, 0)
})
