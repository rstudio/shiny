test_that("a resume starts the session once, with the message's inputs applied", {
  runs <- 0
  seen <- NULL
  server <- function(input, output, session) {
    runs <<- runs + 1
    observe(seen <<- list(plus = input$plus, txt = input$txt))
  }
  start_with(server, '{"method":"resume","data":{"inputs":{"plus:shiny.action":3,"txt":"typed"}}}')
  expect_identical(runs, 1)
  expect_equal(as.numeric(seen$plus), 3)
  expect_s3_class(seen$plus, "shinyActionButtonValue")   # input handlers ran
  expect_identical(seen$txt, "typed")
})

test_that("a resume seeds an inactive RestoreContext that restoreInput() reads", {
  restored <- NULL
  active <- NULL
  server <- function(input, output, session) {
    restored <<- restoreInput("txt", "default")
    active <<- session$restoreContext$active
  }
  start_with(server, '{"method":"resume","data":{"inputs":{"txt":"typed",".clientdata_url_search":""}}}')
  expect_identical(restored, "typed")
  expect_false(active)
})

test_that("a resume does not re-restore the URL's bookmark, and onRestore() stays silent", {
  shinyOptions(bookmarkStore = "url")
  withr::defer(shinyOptions(bookmarkStore = NULL))
  restores <- 0
  seen <- NULL
  server <- function(input, output, session) {
    session$onRestore(function(state) restores <<- restores + 1)
    observe(seen <<- input$txt)
  }
  # The same URL on init restores "bm"; on resume the live "typed" wins.
  url <- '"?_inputs_&txt=%22bm%22"'
  start_with(server, sprintf('{"method":"init","data":{"txt":"typed",".clientdata_url_search":%s}}', url))
  expect_identical(restores, 1)
  restores <- 0
  start_with(server, sprintf('{"method":"resume","data":{"inputs":{"txt":"typed",".clientdata_url_search":%s}}}', url))
  expect_identical(seen, "typed")
  expect_identical(restores, 0)
})

test_that("a resume without inputs starts the session with none", {
  runs <- 0
  server <- function(input, output, session) runs <<- runs + 1
  start_with(server, '{"method":"resume","data":{}}')
  expect_identical(runs, 1)
  runs <- 0
  start_with(server, '{"method":"resume","data":{"inputs":"junk"}}')
  expect_identical(runs, 1)
})

test_that("bookmark-restore file values are dropped from a resume", {
  seen <- "unset"
  server <- function(input, output, session) observe(seen <<- input$f)
  # What the GET path renders into a fileInput()'s data-restore for a
  # server-side bookmark; its file lives in a bookmark directory this
  # session does not have.
  start_with(server, '{"method":"resume","data":{"inputs":{"f:shiny.file":{"name":["a.csv"],"size":[1],"type":["text/csv"],"datapath":["0.csv"]}}}}')
  expect_null(seen)
})

test_that("init is unchanged: the URL's bookmark is restored and onRestore() fires", {
  shinyOptions(bookmarkStore = "url")
  withr::defer(shinyOptions(bookmarkStore = NULL))
  restores <- 0
  restored <- NULL
  server <- function(input, output, session) {
    session$onRestore(function(state) restores <<- restores + 1)
    restored <<- restoreInput("txt", "default")
  }
  start_with(server, '{"method":"init","data":{".clientdata_url_search":"?_inputs_&txt=%22bm%22"}}')
  expect_identical(restored, "bm")
  expect_identical(restores, 1)
})

test_that("with resume off, a reconnect does not replay past events, and the next event fires", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  sends <- list2env(list(n = 0))
  count <- NULL
  server <- function(input, output, session) {
    n <- reactiveVal(0)
    observeEvent(input$plus, n(n() + 1))
    observeEvent(input$send, sends$n <- sends$n + 1)
    observe(count <<- n())
  }
  ws <- start_with(server, '{"method":"resume","data":{"inputs":{"plus:shiny.action":2,"send:shiny.action":1}}}')
  expect_identical(sends$n, 0)
  expect_identical(count, 0)
  ws$callbacks$message(FALSE, '{"method":"update","data":{"plus:shiny.action":3}}')
  flushReact()
  expect_identical(count, 1)
})

test_that("init is unchanged: a fresh page with a non-zero input runs its handler", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  runs <- 0
  server <- function(input, output, session) observeEvent(input$x, runs <<- runs + 1)
  start_with(server, '{"method":"init","data":{"x":"a"}}')
  expect_identical(runs, 1)
})

test_that("with resume off, a reconnect runs onResume() callbacks with the replayed inputs set; init does not", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  seen <- list()
  server <- function(input, output, session) {
    session$onResume(function(info) seen[[length(seen) + 1L]] <<- list(info = info, go = as.numeric(input$go)))
  }
  ws <- start_with(server, '{"method":"resume","data":{"inputs":{"go:shiny.action":2}}}')
  expect_identical(seen, list(list(info = list(restored = FALSE), go = 2)))
  expect_length(sent_of(ws, "resumed"), 0)
  start_with(server, '{"method":"init","data":{"go:shiny.action":2}}')
  expect_length(seen, 1)
})

test_that("with resume off a resume sends no `resumed` and logs nothing", {
  withr::local_envvar(SHINY_RESUME = "FALSE")
  server <- function(input, output, session) observeEvent(input$plus, NULL)
  expect_silent(ws <- start_with(server, '{"method":"resume","data":{"inputs":{"plus:shiny.action":3}}}'))
  expect_length(sent_of(ws, "resumed"), 0)
})
