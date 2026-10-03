withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("ExtendedTask keeps one labelled state value; an anonymous task is internal", {
  s <- snapshot_session(function(input, output, session) {
    task <- ExtendedTask$new(function() promises::promise_resolve(42))
    ExtendedTask$new(function() 1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_identical(names(rec$sources), "|reactiveVal|ExtendedTask task")
  expect_true(rec$complete)
  expect_identical(node_value(rec$sources[[1]])$status, "initial")
})

test_that("a running ExtendedTask restores as an interrupted error; a finished one restores its result", {
  rec_for <- function(state) list(
    format = 2L, complete = TRUE, blocked = list(), inputs = list(), clientData = list(), fileInputs = character(0),
    userData = list(), sources = list("|reactiveVal|ExtendedTask task" = list(kind = "reactiveVal", bytes = serialize(state, NULL, xdr = FALSE)))
  )
  run_with <- function(rec) {
    s <- MockShinySession$new()
    s$.snapshotRegistry$resume(rec, live = list(inputs = list(), clientData = list(), fileInputs = character(0)), fresh = FALSE,
      run = function() withMockContext(s, { task <- ExtendedTask$new(function() 1); s$env$task <- task }))
    s
  }
  s <- run_with(rec_for(list(status = "success", value = list(value = 42, visible = TRUE), error = NULL)))
  expect_identical(isolate(s$env$task$status()), "success")
  expect_identical(isolate(s$env$task$result()), 42)
  s <- run_with(rec_for(list(status = "running", value = NULL, error = NULL)))
  expect_identical(isolate(s$env$task$status()), "error")
  expect_error(isolate(s$env$task$result()), "interrupted by a server restart")
})

test_that("a cached renderPlot's size value is internal", {
  s <- snapshot_session(function(input, output, session) {
    output$p <- renderPlot(plot(1)) |> bindCache(1)
  })
  rec <- s$.snapshotRegistry$snapshot()
  expect_true(rec$complete)
  expect_length(rec$sources, 0)
})
