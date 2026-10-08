withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("onResume() takes a function of one argument and returns an unregister function", {
  s <- MockShinySession$new()
  msg <- "`fun` must be a function that takes one argument"
  expect_error(s$onResume(1), msg, fixed = TRUE)
  expect_error(s$onResume(function() NULL), msg, fixed = TRUE)
  expect_error(s$makeScope("m")$onResume("f"), msg, fixed = TRUE)
  ran <- character(0)
  s$onResume(function(info) ran <<- c(ran, "kept"))
  drop <- s$onResume(function(info) ran <<- c(ran, "dropped"))
  expect_true(is.function(drop))
  drop()
  expect_identical(s$.resumeCallbacks$run(TRUE), 1L)
  expect_identical(ran, "kept")
  expect_identical(s$.resumeCallbacks$run(TRUE), 0L)   # once per session
  expect_identical(ran, "kept")
})

test_that("onResume() never fires in a plain testServer()", {
  ran <- FALSE
  testServer(function(input, output, session) {
    session$onResume(function(info) ran <<- TRUE)
  }, {
    session$setInputs(x = 1)
  })
  expect_false(ran)
})

test_that("a restored resume runs callbacks once, before any observer, seeing restored and merged values", {
  events <- character(0)
  seen <- NULL
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    observe(events <<- c(events, "observer"))
    session$onResume(function(info) {
      events <<- c(events, "callback")
      seen <<- list(info = info, count = count(), plus = as.numeric(input$plus))
    })
  }
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:3) s$setInputs(plus = i), live = list(plus = 4))
  expect_identical(rt$outcome$resumed, "snapshot")
  # The first session's observer; then, in the second, the callback before its observer.
  expect_identical(events, c("observer", "callback", "observer"))
  # count is restored state; plus is the payload's (record-lag) value, merged before the callback.
  expect_identical(seen, list(info = list(restored = TRUE), count = 3, plus = 4))
})

test_that("callbacks run in global registration order, each under the namespace it was registered in", {
  order <- character(0)
  inner <- function(id) moduleServer(id, function(input, output, session) {
    session$onResume(function(info) {
      order <<- c(order, "inner")
      output$built <- renderText("from inner")
      moduleServer("deeper", function(input, output, session) order <<- c(order, session$ns("x")))
    })
  })
  outer <- function(id) moduleServer(id, function(input, output, session) {
    session$onResume(function(info) order <<- c(order, "outer"))
    inner("in")
  })
  server <- function(input, output, session) {
    session$onResume(function(info) order <<- c(order, "root 1"))
    outer("out")
    session$onResume(function(info) order <<- c(order, "root 2"))
    count <- reactiveVal(0)
  }
  rt <- snapshot_roundtrip(server)
  expect_identical(order, c("root 1", "outer", "inner", "out-in-deeper-x", "root 2"))
  expect_identical(rt$second$getOutput("out-in-built"), "from inner")
})

test_that("callbacks registered during the pass run in it; unregistered or later ones do not", {
  order <- character(0)
  panel <- function(id) moduleServer(id, function(input, output, session) {
    session$onResume(function(info) order <<- c(order, paste("panel", id)))
  })
  server <- function(input, output, session) {
    dropLater <- NULL
    session$onResume(function(info) { order <<- c(order, "root"); panel("p1"); dropLater() })
    dropLater <- session$onResume(function(info) order <<- c(order, "dropped"))
    session$onResume(function(info) order <<- c(order, "last"))
    observe(session$onResume(function(info) order <<- c(order, "too late")))
    count <- reactiveVal(0)
  }
  snapshot_roundtrip(server)
  expect_identical(order, c("root", "last", "panel p1"))
})

test_that("panels added by clicks come back live: ids restored, servers rebuilt by onResume(), state restored", {
  starts <- new_spy("p1", "p2")
  panelServer <- function(id) moduleServer(id, function(input, output, session) {
    starts[[id]] <- starts[[id]] + 1
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    output$count <- renderText(paste("Panel count:", count()))
  })
  server <- function(input, output, session) {
    ids <- reactiveVal(character())
    observeEvent(input$add, {
      id <- paste0("p", input$add)
      ids(c(ids(), id))
      insertUI("#panels", ui = div(id = id))
      panelServer(id)
    })
    session$onResume(function(info) for (id in ids()) panelServer(id))
  }
  rt <- snapshot_roundtrip(server, function(s) {
    s$setInputs(add = 1)
    s$setInputs(add = 2)
    s$setInputs(`p1-plus` = 1)
    s$setInputs(`p1-plus` = 2)
    s$setInputs(`p2-plus` = 1)
  })
  s2 <- rt$second
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_identical(isolate(s2$env$ids()), c("p1", "p2"))
  expect_equal(c(starts$p1, starts$p2), c(1, 1))        # each panel server started once: no duplicate
  expect_identical(s2$getOutput("p1-count"), "Panel count: 2")
  expect_identical(s2$getOutput("p2-count"), "Panel count: 1")
  s2$setInputs(`p2-plus` = 2)
  expect_identical(s2$getOutput("p2-count"), "Panel count: 2")
})

test_that("an excluded source rebuilt in onResume() holds its new value by the first flush", {
  seen <- list()
  server <- function(input, output, session) {
    settings <- reactiveVal("light")
    client <- excludeFromResume(reactiveVal(NULL, label = "client"))
    observeEvent(input$theme, { settings(input$theme); client(paste("client for", input$theme)) })
    observe(seen <<- c(seen, list(client())))
    session$onResume(function(info) client(paste("client for", settings())))
  }
  rt <- snapshot_roundtrip(server, function(s) s$setInputs(theme = "dark"))
  expect_identical(rt$outcome$resumed, "snapshot")
  expect_identical(isolate(rt$second$env$client()), "client for dark")
  # One run per session. The second session's first flush already sees the
  # rebuilt client; without the callback it would see NULL.
  expect_identical(seen, list("client for dark", "client for dark"))
})

test_that("a callback that writes a restored source raises no rule-4 message", {
  seen <- character(0)
  local_mocked_bindings(devmode_inform = function(message, ...) seen <<- c(seen, message))
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$plus, count(count() + 1))
    session$onResume(function(info) count(count() * 10))
  }
  rt <- snapshot_roundtrip(server, function(s) for (i in 1:3) s$setInputs(plus = i))
  expect_equal(isolate(rt$second$env$count()), 30)
  expect_false(any(grepl("first flush", seen)))
})
