withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("the record carries sources, key order, inputs, userData and completeness", {
  s <- snapshot_session(function(input, output, session) {
    count <- reactiveVal(3)
    rv <- reactiveValues(z = 1, a = 2)
    session$userData$note <- "kept"
    session$userData$conn <- function() 1
  })
  s$setInputs(n = 5)
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(rec$format, 2L)
  expect_true(rec$complete)
  expect_identical(node_value(rec$sources[["|reactiveVal|count"]]), 3)
  expect_identical(rec$sources[["|reactiveValues|rv"]]$order, c("z", "a"))
  expect_identical(rec$inputs$n, 5)
  expect_identical(rec$userData, list(note = "kept"))
  rep <- rec$report
  expect_identical(rep$reason[rep$kind == "userData" & rep$label == "conn"], "value is not serializable: value")
  expect_true(all(rep$adoptable[rep$kind == "input"]))
})

test_that("non-serializable, over-cap and too-large sources block the record with reasons", {
  local_enable_resume(maxNodeSize = 200)
  s <- snapshot_session(function(input, output, session) {
    fit <- reactiveVal(local({ d <- data.frame(x = 1:5, y = 1:5); lm(y ~ x, d) }))
    big <- reactiveVal(rnorm(1e4))
    deep <- reactiveVal(local({ x <- 1; for (i in 1:60) x <- list(x); x }))
    rv <- reactiveValues(ok = 1, bad = new.env())
    fine <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_false(rec$complete)
  reasons <- stats::setNames(vapply(rec$blocked, `[[`, "", "reason"), vapply(rec$blocked, `[[`, "", "id"))
  expect_identical(unname(reasons["|reactiveVal|fit"]), "value is not serializable: value$terms@.Environment")
  expect_identical(unname(reasons["|reactiveVal|big"]), "over maxNodeSize")
  expect_identical(unname(reasons["|reactiveVal|deep"]), "too large to check")
  expect_identical(unname(reasons["|reactiveValues|rv"]), "value is not serializable: value$bad")
  expect_identical(names(rec$sources), "|reactiveVal|fine")
})

test_that("the total cap blocks the largest sources first", {
  local_enable_resume(maxSize = 3000)
  s <- snapshot_session(function(input, output, session) {
    small <- reactiveVal(1)
    mid <- reactiveVal(rnorm(200))
    large <- reactiveVal(rnorm(300))
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_setequal(names(rec$sources), c("|reactiveVal|small", "|reactiveVal|mid"))
  expect_false(rec$complete)
  expect_identical(rec$blocked[[1]], list(id = "|reactiveVal|large", reason = "over maxSize"))
})

test_that("password inputs are left out of the record and reported", {
  s <- snapshot_session(function(input, output, session) NULL)
  s$setInputs(pw = "hunter2", user = "bob")
  withReactiveDomain(s, setSerializer("pw", serializerUnserializable))
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(names(rec$inputs), "user")
  expect_identical(rec$report$reason[rec$report$label == "pw"], "password input; restored from the client")
})

test_that("a destroyed source is not recorded", {
  s <- snapshot_session(function(input, output, session) {
    moduleServer("m", function(input, output, session) { v <- reactiveVal(1) })
    keep <- reactiveVal(2)
  })
  s$destroy("m")
  expect_identical(names(s$.snapshotRegistry$snapshot()$sources), "|reactiveVal|keep")
})

test_that("resumeReport() groups by reason when printed and is empty with resume off", {
  s <- snapshot_session(function(input, output, session) {
    secret <- reactiveVal(1); disableResume(secret)
    ok <- reactiveVal(2)
  })
  rep <- s$resumeReport()
  expect_s3_class(rep, "shiny_resume_report")
  expect_identical(names(rep), c("kind", "label", "namespace", "adoptable", "reason"))
  expect_output(print(rep), "disableResume\\(\\):\\n  reactiveVal secret")
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), {
    off <- snapshot_session(function(input, output, session) { v <- reactiveVal(1) })
    expect_null(off$.snapshotRegistry)
    expect_identical(nrow(off$resumeReport()), 0L)
  })
})

test_that("a reactiveValues() in userData is reported, not fatal to the record", {
  s <- snapshot_session(function(input, output, session) {
    session$userData$events <- reactiveValues(click = NULL)
    v <- reactiveVal(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_null(rec$userData$events)
  expect_identical(rec$report$reason[rec$report$label == "events"], "value is not serializable: value")
})

test_that("an error while recording one value blocks that value, not the record", {
  s <- snapshot_session(function(input, output, session) {
    bad <- reactiveVal("explodes")
    good <- reactiveVal(1)
    session$userData$bad <- "explodes"
    session$userData$good <- 2
  })
  walk <- snapshot_unserializable_path
  local_mocked_bindings(snapshot_unserializable_path = function(x, ...) {
    if (identical(x, "explodes")) stop("boom")
    walk(x, ...)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_false(rec$complete)
  expect_identical(rec$blocked, list(list(id = "|reactiveVal|bad", reason = "could not be checked: boom")))
  expect_identical(names(rec$sources), "|reactiveVal|good")
  expect_identical(rec$userData, list(good = 2))
  expect_identical(rec$report$reason[rec$report$kind == "userData" & rec$report$label == "bad"], "could not be checked: boom")
})
