withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("sources are identified by namespace, kind and label", {
  s <- snapshot_session(function(input, output, session) {
    count <- reactiveVal(0)
    rv <- reactiveValues(a = 1)
    explicit <- reactiveVal(2, label = "lbl")
    named <- reactiveValues(b = 2, .label = "vals")
  })
  expect_setequal(short_ids(s), c("reactiveVal|count", "reactiveValues|rv", "reactiveVal|lbl", "reactiveValues|vals"))
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_identical(node_value(rec$sources[["|reactiveVal|count"]]), 0)
  expect_identical(node_value(rec$sources[["|reactiveValues|rv"]]), list(a = 1))
  expect_identical(rec$sources[["|reactiveValues|rv"]]$order, "a")
})

test_that("module instances are told apart by namespace", {
  mod <- function(id) moduleServer(id, function(input, output, session) { v <- reactiveVal(id) })
  s <- snapshot_session(function(input, output, session) { mod("one"); mod("two") })
  expect_setequal(names(s$.snapshotRegistry$snapshot()$sources), c("one-|reactiveVal|v", "two-|reactiveVal|v"))
  dup <- snapshot_session(function(input, output, session) { mod("one"); mod("one") })
  rec <- dup$.snapshotRegistry$snapshot()
  expect_false(rec$complete)
  expect_identical(rec$blocked[[1]]$reason, "duplicate label")
  expect_length(rec$sources, 0)
})

test_that("a source with neither label nor assignment is not saved and does not block the record", {
  s <- snapshot_session(function(input, output, session) {
    vals <- lapply(1:2, function(i) reactiveVal(i))      # assignment is to `vals`, not the source
    ok <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_length(rec$blocked, 0)
  expect_identical(names(rec$sources), "|reactiveVal|ok")
  rep <- rec$report
  expect_identical(rep$reason[rep$kind == "reactiveVal" & is.na(rep$label)], c("no label", "no label"))
})

test_that("a marked source without identity does not block; a marked one with identity is not saved", {
  s <- snapshot_session(function(input, output, session) {
    anon <- lapply(1, function(i) excludeFromResume(reactiveVal(i)))
    secret <- reactiveVal("s"); excludeFromResume(secret)
    keep <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_identical(names(rec$sources), "|reactiveVal|keep")
  expect_identical(rec$report$reason[rec$report$label %in% "secret"], "excludeFromResume()")
})

test_that("two sources with one label both lose their identity", {
  s <- snapshot_session(function(input, output, session) {
    for (i in 1:2) {
      rv <- reactiveVal(i)
    }
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_false(rec$complete)
  expect_length(rec$sources, 0)
  expect_identical(vapply(rec$blocked, `[[`, "", "reason"), c("duplicate label", "duplicate label"))
})

test_that("three sources with one label all lose their identity", {
  s <- snapshot_session(function(input, output, session) {
    for (i in 1:3) {
      rv <- reactiveVal(i)
    }
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_length(rec$sources, 0)
  expect_identical(vapply(rec$blocked, `[[`, "", "reason"), rep("duplicate label", 3))
})

test_that("a source created inside a reactive() is exempt: not saved, not blocking", {
  s <- snapshot_session(function(input, output, session) {
    r <- reactive({ inner <- reactiveVal(1); inner() })
    out <- isolate(r())
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_length(rec$sources, 0)
  expect_identical(rec$report$reason[rec$report$label %in% "inner"], "created inside a reactive()")
})

test_that("without srcrefs a labelled source has an identity and an unlabelled one never blocks", {
  s <- MockShinySession$new()
  withMockContext(s, snapshot_eval_no_srcref('
    b <- reactiveVal(2, label = "b")
    lapply(1, function(i) reactiveVal(i))
  '))
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(names(rec$sources), "|reactiveVal|b")
  expect_true(rec$complete)
})

test_that("a reactiveValues record restores saved keys and keeps new initial keys", {
  s1 <- snapshot_session(function(input, output, session) { rv <- reactiveValues(a = 1, b = 2) })
  isolate(s1$env$rv$a <- 10)
  rec <- snapshot_decode_sources(s1$.snapshotRegistry$snapshot())
  s2 <- MockShinySession$new()
  reg <- s2$.snapshotRegistry
  reg$resume(rec, live = list(inputs = list(), clientData = list()),
             run = function() withMockContext(s2, { rv <- reactiveValues(a = 1, c = 3); s2$env$rv <- rv }))
  expect_identical(isolate(reactiveValuesToList(s2$env$rv)), list(a = 10, b = 2, c = 3))
})

test_that("Shiny's own sources are marked internal and never reported", {
  s <- snapshot_session(function(input, output, session) {
    r <- reactive(input$x)
    d <- debounce(r, 100)
    t <- throttle(r, 100)
    p <- reactivePoll(100, session, function() 1, function() 1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_length(rec$sources, 0)
  expect_false(any(grepl("debounce|throttle|cookie", rec$report$label)))
})

test_that("devmode names unlabelled sources once, by their calls, only if still unmarked when the record is taken", {
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  s <- snapshot_session(function(input, output, session) {
    marked <- lapply(1, function(i) excludeFromResume(reactiveVal(i)))
    output$p <- renderPlot(plot(1)) |> bindCache(1)
  })
  s$.snapshotRegistry$snapshot()
  expect_false(any(grepl("with no label", seen)))
  s2 <- snapshot_session(function(input, output, session) {
    vals <- lapply(1:2, function(i) reactiveVal(i))
  })
  s2$.snapshotRegistry$snapshot()
  s2$.snapshotRegistry$snapshot()
  hits <- grep("with no label", seen, value = TRUE)
  expect_length(hits, 1)
  expect_match(hits, "^2 sources with no label were not saved: reactiveVal\\(")   # Task 7 adds the call text
  expect_match(hits, "rule 1", fixed = TRUE)
})

test_that("without srcrefs, sources are labelled from the caller's body: unique, ordered top-level twins, per module instance", {
  env <- snapshot_eval_no_srcref('
    mod <- function(id) moduleServer(id, function(input, output, session) {
      history <- reactiveVal(list())
      x <- reactiveVal(0)
      y <- reactiveVal(0)
      if (TRUE) branchy <- reactiveVal(1) else branchy <- reactiveVal(1)
      helper <- function() reactiveVal(2)
      h <- helper()
      listed <- list(z = reactiveVal(3))
    })
    server <- function(input, output, session) { mod("a"); mod("b") }
  ', new.env())
  s <- snapshot_session(env$server)
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_setequal(names(rec$sources), c(
    "a-|reactiveVal|history", "a-|reactiveVal|x", "a-|reactiveVal|y",
    "b-|reactiveVal|history", "b-|reactiveVal|x", "b-|reactiveVal|y"
  ))
  rep <- rec$report
  expect_identical(sum(rep$reason %in% "no label"), 6L)   # branchy, helper, listed, in each instance
})

test_that("a body-labelled source restores across sessions like a srcref-labelled one", {
  env <- snapshot_eval_no_srcref('
    server <- function(input, output, session) {
      history <- reactiveVal(character(0))
      observeEvent(input$add, history(c(history(), input$add)))
    }
  ', new.env())
  rt <- snapshot_roundtrip(env$server, function(s) { s$setInputs(add = "a"); s$setInputs(add = "b") })
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_identical(isolate(rt$second$env$history()), c("a", "b"))
})

test_that("a loop assigning one name gives a duplicate label, as it does with srcrefs", {
  env <- snapshot_eval_no_srcref('
    server <- function(input, output, session) { for (i in 1:2) rv <- reactiveVal(i) }
  ', new.env())
  rec <- snapshot_session(env$server)$.snapshotRegistry$snapshot()
  expect_identical(vapply(rec$blocked, `[[`, "", "reason"), c("duplicate label", "duplicate label"))
})

test_that("devmode names unlabelled sources by their calls", {
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  s <- snapshot_session(function(input, output, session) { vals <- lapply(1:2, function(i) reactiveVal(i)) })
  s$.snapshotRegistry$snapshot()
  expect_match(seen, "not saved: reactiveVal(i), reactiveVal(i).", fixed = TRUE, all = FALSE)
})

test_that("reactiveValues() is labelled from the caller's body too", {
  env <- snapshot_eval_no_srcref('
    server <- function(input, output, session) { state <- reactiveValues(n = 0) }
  ', new.env())
  expect_identical(short_ids(snapshot_session(env$server)), "reactiveValues|state")
})

test_that("excludeFromResume() marks each kind of source and returns it invisibly", {
  s <- snapshot_session(function(input, output, session) {
    v <- excludeFromResume(reactiveVal(1, label = "v"))
    rv <- excludeFromResume(reactiveValues(a = 1, .label = "rv"))
    keep <- reactiveVal(2)
  })
  expect_identical(withVisible(excludeFromResume(s$env$v))$visible, FALSE)
  expect_identical(excludeFromResume(s$env$v), s$env$v)
  expect_identical(withVisible(excludeFromResume(s$env$rv))$visible, FALSE)
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(names(rec$sources), "|reactiveVal|keep")
  expect_identical(rec$report$reason[rec$report$label %in% c("v", "rv")], rep("excludeFromResume()", 2))
})

test_that("excludeFromResume() rejects observers and anything else it has no method for", {
  expect_error(excludeFromResume(42),
    "excludeFromResume() expects a reactiveVal, reactiveValues object or ExtendedTask, not an object of class numeric.", fixed = TRUE)
  o <- observe(NULL)
  withr::defer(o$destroy())
  expect_error(excludeFromResume(o), "not an object of class Observer/R6.", fixed = TRUE)
})

test_that("with resume off excludeFromResume() returns the source unchanged", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  s <- snapshot_session(function(input, output, session) v <- excludeFromResume(reactiveVal(1)))
  expect_null(s$.snapshotRegistry)
  expect_equal(isolate(s$env$v()), 1)
})

test_that("resume-off constructors do not inspect call sites", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  local_mocked_bindings(
    snapshot_call_site = function(...) stop("call site inspected"),
    snapshot_source_label = function(...) stop("identity inspected")
  )
  s <- snapshot_session(function(input, output, session) {
    v <- reactiveVal(1)
    rv <- reactiveValues(a = 2)
  })
  expect_identical(isolate(s$env$v()), 1)
  expect_identical(isolate(s$env$rv$a), 2)
})

test_that("explicit source labels do not inspect call sites", {
  local_mocked_bindings(snapshot_call_site = function(...) stop("call site inspected"))
  s <- snapshot_session(function(input, output, session) {
    v <- reactiveVal(1, label = "v")
    rv <- reactiveValues(a = 2, .label = "rv")
  })
  expect_setequal(short_ids(s), c("reactiveVal|v", "reactiveValues|rv"))
})

test_that("tracking a source does not retain its caller's frame", {
  s <- MockShinySession$new()
  make <- function() {
    payload <- new.env(parent = emptyenv())
    ref <- rlang::new_weakref(payload)
    source <- reactiveVal(0, label = "small")
    list(source = source, ref = ref)
  }
  held <- withMockContext(s, make())
  invisible(gc())
  expect_null(rlang::wref_key(held$ref))
  expect_identical(isolate(held$source()), 0)
})

test_that("restore callbacks do not retain replaced constructor values", {
  s <- MockShinySession$new()
  make <- function(kind) {
    payload <- new.env(parent = emptyenv())
    ref <- rlang::new_weakref(payload)
    if (kind == "reactiveVal") {
      source <- attr(reactiveVal(payload, label = "v"), ".impl", exact = TRUE)
      source$set(NULL)
    } else {
      source <- reactiveValues(payload = payload, .label = "rv")
      source$payload <- NULL
    }
    list(source = source, ref = ref)
  }
  for (kind in c("reactiveVal", "reactiveValues")) {
    held <- withMockContext(s, make(kind))
    invisible(gc())
    expect_null(rlang::wref_key(held$ref))
  }
})
