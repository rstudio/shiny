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

test_that("a source with neither label nor assignment has no identity and blocks the record", {
  s <- snapshot_session(function(input, output, session) {
    vals <- lapply(1:2, function(i) reactiveVal(i))      # assignment is to `vals`, not the source
    ok <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_false(rec$complete)
  expect_identical(unique(vapply(rec$blocked, `[[`, "", "reason")), "no label")
  expect_identical(names(rec$sources), "|reactiveVal|ok")
  rep <- rec$report
  expect_identical(rep$reason[rep$kind == "reactiveVal" & is.na(rep$label)], c("no label", "no label"))
})

test_that("a marked source without identity does not block; a marked one with identity is not saved", {
  s <- snapshot_session(function(input, output, session) {
    anon <- lapply(1, function(i) disableResume(reactiveVal(i)))
    secret <- reactiveVal("s"); disableResume(secret)
    keep <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_identical(names(rec$sources), "|reactiveVal|keep")
  expect_identical(rec$report$reason[rec$report$label %in% "secret"], "disableResume()")
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

test_that("without srcrefs an unlabelled source has no identity and a labelled one does", {
  s <- MockShinySession$new()
  withMockContext(s, snapshot_eval_no_srcref('
    a <- reactiveVal(1)
    b <- reactiveVal(2, label = "b")
  '))
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(names(rec$sources), "|reactiveVal|b")
  expect_false(rec$complete)
})

test_that("a reactiveValues record restores saved keys and keeps new initial keys", {
  s1 <- snapshot_session(function(input, output, session) { rv <- reactiveValues(a = 1, b = 2) })
  isolate(s1$env$rv$a <- 10)
  rec <- s1$.snapshotRegistry$snapshot()
  s2 <- MockShinySession$new()
  reg <- s2$.snapshotRegistry
  reg$resume(rec, live = list(inputs = list(), clientData = list(), fileInputs = character(0)), fresh = FALSE,
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
