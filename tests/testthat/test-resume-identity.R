# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

test_that("sources get stable, label-keyed ids across two sessions", {
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    rv <- reactiveValues(a = 1)
    labelled <- reactiveVal(0, label = "explicit")
  }
  ids1 <- snapshot_short_ids(snapshot_session(server))
  ids2 <- snapshot_short_ids(snapshot_session(server))
  expect_identical(ids1, ids2)
  expect_true("reactiveVal:count#1" %in% ids1)
  expect_true("reactiveValues:rv#1" %in% ids1)
  expect_true("reactiveVal:explicit#1" %in% ids1)
  expect_true("input" %in% snapshot_session(server)$.snapshotRegistry$ids())
})

test_that("ids carry the namespace and the creating node", {
  server <- function(input, output, session) {
    moduleServer("m", function(input, output, session) {
      x <- reactiveVal(0)
    })
  }
  s <- snapshot_session(server)
  ids <- s$.snapshotRegistry$ids()
  expect_true(any(grepl("^m-\\|top\\|reactiveVal:x#1$", ids)))
})

test_that("duplicate labels under one creator get positional occurrence numbers", {
  server <- function(input, output, session) {
    vals <- lapply(1:3, function(i) reactiveVal(i, label = "loop"))
  }
  ids <- snapshot_short_ids(snapshot_session(server))
  expect_true(all(c("reactiveVal:loop#1", "reactiveVal:loop#2", "reactiveVal:loop#3") %in% ids))
})

test_that("reactiveValues(.label) is identity-bearing and not a value", {
  server <- function(input, output, session) {
    rv <- reactiveValues(label = "i am a value", .label = "state")
  }
  s <- snapshot_session(server)
  expect_true("reactiveValues:state#1" %in% snapshot_short_ids(s))
  expect_identical(isolate(s$env$rv$label), "i am a value")
  expect_identical(isolate(names(s$env$rv)), "label")
})

test_that("a vector label is collapsed into one identity label instead of raising", {
  server <- function(input, output, session) {
    v <- reactiveVal(1, label = c("a", "b"))
    rv <- reactiveValues(x = 1, .label = c("s", "t"))
  }
  s <- snapshot_session(server)
  s$flushReact()
  ids <- snapshot_short_ids(s)
  expect_true(all(c("reactiveVal:a, b#1", "reactiveValues:s, t#1") %in% ids))
})

test_that("without srcrefs, a source's identity is a hash of its constructor call, stable across loads", {
  code <- 'server <- function(input, output, session) {
    a <- reactiveVal(1)
    b <- reactiveVal(1)
    rv <- reactiveValues(x = 1)
  }'
  env1 <- snapshot_eval_no_srcref(code, new.env())
  env2 <- snapshot_eval_no_srcref(code, new.env())
  ids1 <- snapshot_short_ids(snapshot_session(env1$server))
  ids2 <- snapshot_short_ids(snapshot_session(env2$server))
  expect_identical(ids1, ids2)
  # Identical calls collapse to one label and are told apart positionally.
  expect_true(any(grepl("^reactiveVal:[0-9a-f]{32}#1$", ids1)))
  expect_true(any(grepl("^reactiveVal:[0-9a-f]{32}#2$", ids1)))
  expect_false(any(grepl("reactiveVal:a#", ids1)))
  a_ids <- grep("^reactiveVal:", ids1, value = TRUE)
  expect_length(a_ids, 2)
})

test_that("a source adopts a record's value at construction, before anything reads it", {
  s <- MockShinySession$new()
  reg <- s$.snapshotRegistry
  reg$.__enclos_env__$private$records <- list(
    "mock-session-|top|reactiveVal:count#1" = list(hasValue = TRUE, bytes = serialize(42, NULL)),
    "mock-session-|top|reactiveValues:rv#1" = list(hasValue = TRUE, bytes = serialize(list(b = 2, a = 9), NULL), order = c("b", "a"))
  )
  reg$.__enclos_env__$private$adopting <- TRUE
  server <- function(input, output, session) {
    count <- reactiveVal(0)
    rv <- reactiveValues(a = 1)
  }
  snapshot_session(server, s)
  expect_identical(isolate(s$env$count()), 42)
  expect_identical(isolate(s$env$rv$a), 9)
  expect_identical(isolate(names(s$env$rv)), c("b", "a"))
})

test_that("a source's entry records how it was labelled and the shape of its initial value", {
  code <- 'server <- function(input, output, session) {
    a <- reactiveVal(NULL)
    rv <- reactiveValues(x = 1, y = 2)
  }'
  env <- snapshot_eval_no_srcref(code, new.env())
  s <- snapshot_session(env$server)
  reg <- s$.snapshotRegistry
  a <- reg$nodeEntry(grep("reactiveVal:", reg$ids(), value = TRUE))
  expect_identical(a$labelSource, "hash")
  expect_identical(a$shape, list(class = "NULL", type = "NULL"))
  rv <- reg$nodeEntry(grep("reactiveValues:", reg$ids(), value = TRUE))
  expect_identical(rv$shape, list(keys = c("x", "y")))
  labelled <- snapshot_session(function(input, output, session) { count <- reactiveVal(0) })
  e <- labelled$.snapshotRegistry$nodeEntry(grep("reactiveVal:count", labelled$.snapshotRegistry$ids(), value = TRUE))
  expect_identical(e$labelSource, "srcref")
  expect_identical(e$shape, list(class = "numeric", type = "double"))
})

test_that("MockShinySession and ShinySession both expose .snapshotRegistry", {
  expect_true(".snapshotRegistry" %in% names(ShinySession$public_fields))
  expect_s3_class(MockShinySession$new()$.snapshotRegistry, "SnapshotRegistry")
})

test_that("snapshot_registry() resolves through module proxies and tolerates foreign domains", {
  s <- MockShinySession$new()
  expect_identical(snapshot_registry(s), s$.snapshotRegistry)
  expect_identical(snapshot_registry(s$makeScope("m")), s$.snapshotRegistry)
  expect_null(snapshot_registry(NULL))
  expect_null(snapshot_registry(new.env()))
  junk <- new.env()
  junk$.snapshotRegistry <- "not a registry"
  expect_null(snapshot_registry(junk))
})

test_that("a domain whose `$` rejects unknown names has no registry", {
  Strict <- methods::setRefClass("StrictDomain", fields = list(x = "numeric"), where = environment())
  expect_null(snapshot_registry(Strict$new(x = 1)))
})

test_that("assigned and explicitly labelled observers use the label; unassigned ones hash their parts", {
  server <- function(input, output, session) {
    o <- observe({ input$a })
    observe({ input$b }, label = "explicit")
    observe({ input$c })
    observeEvent(input$go, { input$d })
    handler <- observeEvent(input$go, { input$e })
    output$txt <- renderText("hi")
  }
  s <- snapshot_session(server)
  ids <- snapshot_short_ids(s)
  expect_true("observer:o#1" %in% ids)
  expect_true("observer:explicit#1" %in% ids)
  expect_true("observer:handler#1" %in% ids)
  expect_true("output:output$txt#1" %in% ids)
  hashed <- grep("^observer:[0-9a-f]{32}#1$", ids, value = TRUE)
  expect_length(hashed, 2)
  expect_identical(ids, snapshot_short_ids(snapshot_session(server)))
})

test_that("observer ids are finalized in construction order, not run order", {
  server <- function(input, output, session) {
    observe({ input$a }, label = "same", priority = 0)
    observe({ input$a }, label = "same", priority = 10)
  }
  s <- snapshot_session(server)
  reg <- s$.snapshotRegistry
  s$flushReact()  # the priority-10 observer runs first
  ids <- snapshot_short_ids(s)
  expect_identical(grep("observer:same#", ids, value = TRUE), c("observer:same#1", "observer:same#2"))
  o2 <- reg$nodeEntry(grep("observer:same#2$", reg$ids(), value = TRUE))
  expect_equal(rlang::wref_key(o2$node)$.priority, 10)
})

test_that("bindEvent() appends the event expression and flags to the hash parts", {
  h <- function(expr, ...) {
    server <- rlang::inject(function(input, output, session) {
      o <- observeEvent(!!expr, { input$x }, ...)
    })
    s <- snapshot_session(server)
    rlang::hash(snapshot_observer_parts(s$env$o))
  }
  base <- h(quote(input$a))
  expect_identical(base, h(quote(input$a)))
  expect_false(identical(base, h(quote(input$b))))
  expect_false(identical(base, h(quote(input$a), ignoreInit = TRUE)))
  expect_false(identical(base, h(quote(input$a), once = TRUE)))
})

test_that("debounce() appends millis to its tracker's hash parts", {
  server <- function(input, output, session) {
    r <- reactive(input$x)
    d <- debounce(r, 500)
  }
  s <- snapshot_session(server)
  reg <- s$.snapshotRegistry
  tracker <- reg$nodeEntry(grep("observer:debounce d tracker#1$", reg$ids(), value = TRUE))
  parts <- snapshot_observer_parts(rlang::wref_key(tracker$node))
  expect_identical(parts$millis, 500)
})

test_that("snapshot_hazards() finds <<- and assign() targets", {
  expect_identical(snapshot_hazards(quote({ a <- 1; conn <<- db(); assign("n", 2); local({ m <<- 3 }) })), c("conn", "n", "m"))
  expect_identical(snapshot_hazards(quote(x <- 1)), character(0))
  expect_identical(snapshot_hazards(rlang::quo(conn <<- 1)), "conn")
  server <- function(input, output, session) {
    conn <- NULL
    o <- observe({ conn <<- input$x })
  }
  expect_identical(snapshot_observer_hazards(snapshot_session(server)$env$o), "conn")
})

test_that("an output's hash part follows the user's render expression", {
  hp <- function(expr) {
    server <- rlang::inject(function(input, output, session) {
      output$txt <- renderText(!!expr)
    })
    s <- snapshot_session(server)
    reg <- s$.snapshotRegistry
    e <- reg$nodeEntry(grep("output:output\\$txt#1$", reg$ids(), value = TRUE))
    snapshot_observer_parts(rlang::wref_key(e$node))
  }
  expect_identical(hp(quote(input$a)), hp(quote(input$a)))
  expect_false(identical(hp(quote(input$a)), hp(quote(input$b))))
})

test_that("a node created inside a reactive() body is registered not adoptable", {
  server <- function(input, output, session) {
    r <- reactive({ inner <- reactiveVal(1); inner() })
    o <- observe({ r() })
  }
  s <- snapshot_session(server)
  s$flushReact()
  reg <- s$.snapshotRegistry
  id <- grep("reactiveVal:inner#1$", reg$ids(), value = TRUE)
  expect_length(id, 1)
  expect_true(startsWith(sub("^[^|]*\\|", "", id), "context:"))
  expect_false(reg$nodeEntry(id)$adoptable)
  expect_identical(reg$nodeEntry(id)$reason, "created inside a reactive()")
})

test_that("a source created inside an observe() body has that observer as its creator", {
  server <- function(input, output, session) {
    o <- observe({ inner <- reactiveVal(1); inner() })
  }
  s <- snapshot_session(server)
  s$flushReact()
  reg <- s$.snapshotRegistry
  id <- grep("reactiveVal:inner#1$", reg$ids(), value = TRUE)
  expect_length(id, 1)
  expect_match(id, "\\|observer:o#1\\|reactiveVal:inner#1$")
  expect_false(grepl("context:", id))
  expect_true(reg$nodeEntry(id)$adoptable)
})

test_that("devmode informs about sources inside reactive() but not inside observe()", {
  withr::local_options(shiny.devmode = TRUE)
  withr::local_envvar(TESTTHAT = "false")
  msgs <- function(server) {
    out <- character(0)
    withCallingHandlers({
      s <- snapshot_session(server)
      s$flushReact()
    }, message = function(m) {
      out <<- c(out, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    out
  }
  inObserver <- function(input, output, session) {
    o <- observe({ inner <- reactiveVal(1); inner() })
  }
  inReactive <- function(input, output, session) {
    r <- reactive({ inner <- reactiveVal(1); inner() })
    o <- observe({ r(); r() })
    o2 <- observe({ r() })
  }
  expect_false(any(grepl("created inside the reactive", msgs(inObserver))))
  out <- msgs(inReactive)
  expect_length(grep("created inside the reactive", out), 1)
})

test_that("constructing reactives, observers and outputs does no deparse or hazard work until a snapshot needs it", {
  server <- function(input, output, session) {
    r <- reactive(input$x + 1)
    o <- observe({ r() })
    e <- observeEvent(input$go, { conn <<- input$x })
    output$txt <- renderText(r())
    d <- debounce(r, function() 500)
  }
  calls <- 0
  real_deparse <- snapshot_deparse
  real_hazards <- snapshot_hazards
  local_mocked_bindings(
    snapshot_deparse = function(x) { calls <<- calls + 1; real_deparse(x) },
    snapshot_hazards = function(expr) { calls <<- calls + 1; real_hazards(expr) }
  )
  s <- snapshot_session(server)
  expect_equal(calls, 0)
  s$setInputs(x = 1, go = 1)
  rep <- s$resumeReport()
  expect_gt(calls, 0)
  expect_true(all(rep$adoptable[rep$kind %in% c("observer", "output")]))
  expect_identical(rep$hazard[grepl("observer:e#1$", rep$id)], "conn")
})
