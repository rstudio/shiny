# The registry exists only in sessions with the feature on; the suite also
# runs with SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

record_for <- function(session, pattern) {
  recs <- session$.snapshotRegistry$observerRecords()
  hit <- grep(pattern, names(recs), value = TRUE)
  expect_length(hit, 1)
  recs[[hit]]
}

test_that("an observer's transitive source set crosses conductors and records path hashes", {
  server <- function(input, output, session) {
    a <- reactiveVal(1)
    rv <- reactiveValues(k = 2)
    r1 <- reactive(a() + rv$k)
    r2 <- reactive(r1() * input$n)
    o <- observe({ r2(); names(rv) })
  }
  s <- snapshot_session(server)
  s$setInputs(n = 3)
  rec <- record_for(s, "observer:o#1$")
  expect_true(rec$upToDate)
  expect_true(rec$adoptable)
  ids <- vapply(rec$sources, `[[`, character(1), "id")
  parts <- vapply(rec$sources, `[[`, character(1), "part")
  keys <- vapply(rec$sources, function(x) x$key %||% NA_character_, character(1))
  expect_setequal(sub("^[^|]*\\|[^|]*\\|", "", ids), c("reactiveVal:a#1", "reactiveValues:rv#1", "input", "reactiveValues:rv#1"))
  expect_true(any(parts == "key" & keys == "n" & ids == "input"))
  expect_true(any(parts == "names"))
  expect_length(rec$pathHashes, 2)
  expect_identical(rec$pathHashes, sort(unique(c(
    snapshot_hash_expr(fn_body(attr(s$env$r1, "observable")$.origFunc)),
    snapshot_hash_expr(fn_body(attr(s$env$r2, "observable")$.origFunc))
  ))))
  expect_true(s$.snapshotRegistry$hasConductorHashes(rec$pathHashes))
  expect_false(s$.snapshotRegistry$hasConductorHashes("nope"))
})

test_that("the queue of unhashed conductor bodies stays bounded in a session that never resumes", {
  s <- MockShinySession$new()
  withReactiveDomain(s, {
    first <- reactive("first")
    for (i in 1:2500) local(reactive(i))
  })
  pending <- s$.snapshotRegistry$.__enclos_env__$private$pendingConductors
  expect_lt(pending$size(), 1000)
  expect_true(s$.snapshotRegistry$hasConductorHashes(
    snapshot_hash_expr(fn_body(attr(first, "observable")$.origFunc))
  ))
})

test_that("body and path hashes change with the code, and only with the code", {
  make <- function(body_expr, cond_expr) {
    rlang::inject(function(input, output, session) {
      r <- reactive(!!cond_expr)
      o <- observe({ r(); !!body_expr })
    })
  }
  rec <- function(srv) { s <- snapshot_session(srv); s$flushReact(); record_for(s, "observer:o#1$") }
  base <- rec(make(quote(1), quote(input$x)))
  same <- rec(make(quote(1), quote(input$x)))
  expect_identical(base$bodyHash, same$bodyHash)
  expect_identical(base$pathHashes, same$pathHashes)
  expect_false(identical(base$bodyHash, rec(make(quote(2), quote(input$x)))$bodyHash))
  expect_false(identical(base$pathHashes, rec(make(quote(1), quote(input$y)))$pathHashes))
})

test_that("non-graph and out-of-session dependencies make an observer not adoptable", {
  global_rv <- reactiveVal(1)
  server <- function(input, output, session) {
    timer <- observe({ invalidateLater(1000); input$x })
    r <- reactive({ invalidateLater(500); 1 })
    via_conductor <- observe({ r() })
    foreign <- observe({ global_rv() })
  }
  s <- snapshot_session(server)
  s$flushReact()
  expect_false(record_for(s, "observer:timer#1$")$adoptable)
  expect_match(record_for(s, "observer:timer#1$")$reason, "timer")
  expect_false(record_for(s, "observer:via_conductor#1$")$adoptable)
  expect_false(record_for(s, "observer:foreign#1$")$adoptable)
  expect_match(record_for(s, "observer:foreign#1$")$reason, "outside the session")
})

test_that("an observer that created other nodes is recorded as such and its children name it", {
  server <- function(input, output, session) {
    ctor <- observe({ child <- reactiveVal(1) })
    plain <- observe({ input$x })
  }
  s <- snapshot_session(server)
  s$flushReact()
  reg <- s$.snapshotRegistry
  expect_true(record_for(s, "observer:ctor#1$")$createsNodes)
  expect_false(record_for(s, "observer:plain#1$")$createsNodes)
  ctor_id <- grep("observer:ctor#1$", reg$ids(), value = TRUE)
  expect_true(any(grepl(paste0("\\|\\Q", ctor_id, "\\E\\|reactiveVal:child#1$"), reg$ids(), perl = TRUE)))
})

test_that("records carry up-to-date, destroyed, suspended and hazard state", {
  server <- function(input, output, session) {
    done <- observe({ done$destroy() })
    paused <- observe({ input$x }, suspended = TRUE)
    n <- 0
    hz <- observe({ n <<- n + 1 })
    out <- observe({ input$y })
  }
  s <- snapshot_session(server)
  s$flushReact()
  s$.snapshotRegistry$finalizeNodes()
  expect_true(record_for(s, "observer:done#1$")$destroyed)
  expect_true(record_for(s, "observer:paused#1$")$suspended)
  expect_false(record_for(s, "observer:paused#1$")$upToDate)
  expect_identical(record_for(s, "observer:hz#1$")$hazard, "n")
  s$setInputs(y = 1)
  expect_true(record_for(s, "observer:out#1$")$upToDate)
})

test_that("an observer mid-await, or one downstream of a conductor mid-await, is not up to date", {
  resolve_p <- NULL
  server <- function(input, output, session) {
    p <- promises::promise(function(resolve, reject) resolve_p <<- resolve)
    slow <- observe({ input$a; p$then(function(x) input$b) })
    r <- reactive({ input$a; p$then(function(x) input$b) })
    via_r <- observe({ r() })
    plain <- observe({ input$a })
  }
  s <- snapshot_session(server)
  s$setInputs(a = 1, b = 2)
  expect_false(record_for(s, "observer:slow#1$")$upToDate)
  expect_true(record_for(s, "observer:slow#1$")$adoptable)
  expect_false(record_for(s, "observer:via_r#1$")$upToDate)
  expect_true(record_for(s, "observer:plain#1$")$upToDate)
  resolve_p(NULL)
  for (i in 1:3) later::run_now()
  s$flushReact()
  rec <- record_for(s, "observer:slow#1$")
  expect_true(rec$upToDate)
  expect_setequal(vapply(rec$sources, `[[`, character(1), "key"), c("a", "b"))
})

test_that("snapshot() records source values, key order, inputs, clientData-less mock, and userData", {
  server <- function(input, output, session) {
    count <- reactiveVal(3)
    rv <- reactiveValues(b = 2, a = 1)
    session$userData$note <- "kept"
    session$userData$conn <- function() NULL
  }
  s <- snapshot_session(server)
  s$setInputs(n = 5, pw = "secret")
  .subset2(s$input, "impl")$setMeta("pw", "shiny.serializer", serializerUnserializable)
  snap <- s$.snapshotRegistry$snapshot()
  expect_identical(snap$format, 1L)
  ids <- sub("^[^|]*\\|[^|]*\\|", "", names(snap$nodes))
  expect_identical(node_value(snap$nodes[[which(ids == "reactiveVal:count#1")]]), 3)
  rvrec <- snap$nodes[[which(ids == "reactiveValues:rv#1")]]
  expect_identical(rvrec$order, c("b", "a"))
  expect_identical(node_value(rvrec), list(b = 2, a = 1))
  expect_identical(snap$inputs$n, 5)
  expect_null(snap$inputs$pw)
  expect_identical(snap$userData, list(note = "kept"))
  rep <- snap$report
  expect_s3_class(rep, "shiny_resume_report")
  expect_identical(names(rep), c("id", "kind", "label", "namespace", "adoptable", "reason", "hazard"))
  expect_identical(rep$reason[rep$id == "input$pw"], "password input; restored from the client")
  expect_identical(rep$reason[rep$id == "userData$conn"], "value is not serializable: value")
})

test_that("source records carry the sibling count and initial shape of their group", {
  code <- 'server <- function(input, output, session) {
    a <- reactiveVal(NULL); b <- reactiveVal(NULL); c <- reactiveVal(NULL)
    only <- reactiveVal(1)
  }'
  env <- snapshot_eval_no_srcref(code, new.env())
  snap <- snapshot_session(env$server)$.snapshotRegistry$snapshot()
  recs <- snap$nodes[grepl("reactiveVal:", names(snap$nodes))]
  siblings <- vapply(recs, `[[`, integer(1), "siblings")
  expect_identical(sort(unname(siblings)), c(1L, 3L, 3L, 3L))
  expect_true(all(vapply(recs, function(r) identical(r$labelSource, "hash"), logical(1))))
  three <- recs[siblings == 3L]
  expect_true(all(vapply(three, function(r) identical(r$shape, list(class = "NULL", type = "NULL")), logical(1))))
})

test_that("non-serializable, over-cap and skipped sources are excluded whole, with reasons", {
  local_enable_resume(maxNodeSize = 200)
  server <- function(input, output, session) {
    fit <- reactiveVal(local({ d <- data.frame(x = 1:5, y = 1:5); lm(y ~ x, d) }))
    big <- reactiveVal(rnorm(1e4))
    skipped <- reactiveVal(1)
    disableResume(skipped)
    rv <- reactiveValues(ok = 1, bad = new.env())
    fine <- reactiveVal(1)
    session$userData$huge <- rnorm(1e4)
  }
  s <- snapshot_session(server)
  snap <- s$.snapshotRegistry$snapshot()
  rep <- snap$report
  row <- function(tail) rep[grepl(paste0("\\Q", tail, "\\E$"), rep$id, perl = TRUE), ]
  expect_false(row("reactiveVal:fit#1")$adoptable)
  expect_identical(row("reactiveVal:fit#1")$reason, "value is not serializable: value$terms@.Environment")
  expect_match(row("reactiveVal:big#1")$reason, "^value exceeds maxNodeSize \\([0-9]+ bytes\\)$")
  expect_match(row("userData$huge")$reason, "^value exceeds maxNodeSize \\([0-9]+ bytes\\)$")
  expect_identical(row("reactiveVal:skipped#1")$reason, "disableResume()")
  expect_identical(row("reactiveValues:rv#1")$reason, "value is not serializable: value$bad")
  expect_true(row("reactiveVal:fine#1")$adoptable)
  expect_true(is.na(row("reactiveVal:fine#1")$reason))
  kept <- sub("^[^|]*\\|[^|]*\\|", "", names(snap$nodes))
  expect_setequal(kept, "reactiveVal:fine#1")
})

test_that("the total cap evicts largest-first", {
  local_enable_resume(maxSize = 3000)
  server <- function(input, output, session) {
    small <- reactiveVal(1)
    mid <- reactiveVal(rnorm(200))
    large <- reactiveVal(rnorm(300))
  }
  s <- snapshot_session(server)
  snap <- s$.snapshotRegistry$snapshot()
  kept <- sub("^[^|]*\\|[^|]*\\|", "", names(snap$nodes))
  expect_true("reactiveVal:small#1" %in% kept)
  expect_true("reactiveVal:mid#1" %in% kept)
  expect_false("reactiveVal:large#1" %in% kept)
  rep <- snap$report
  expect_identical(rep$reason[grepl("reactiveVal:large#1$", rep$id)], "evicted under maxSize")
})

test_that("disableResume() marks observers and render functions; report and record agree", {
  server <- function(input, output, session) {
    o <- observe({ input$x })
    disableResume(o)
    output$clock <- renderText(format(Sys.time())) |> disableResume()
    output$plain <- renderText("x")
  }
  s <- snapshot_session(server)
  s$flushReact()
  rep <- s$resumeReport()
  expect_identical(rep$reason[grepl("observer:o#1$", rep$id)], "disableResume()")
  expect_identical(rep$reason[grepl("output:output\\$clock#1$", rep$id)], "disableResume()")
  expect_true(rep$adoptable[grepl("output:output\\$plain#1$", rep$id)])
  expect_error(disableResume(42), "disableResume")
  expect_snapshot(print(rep[order(rep$id), ]))
})

test_that("the report has one row per node and its hazard column", {
  server <- function(input, output, session) {
    n <- 0
    hz <- observe({ n <<- n + 1; assign("m", 1) })
    rv <- reactiveVal(1)
  }
  s <- snapshot_session(server)
  s$flushReact()
  rep <- s$resumeReport()
  expect_identical(rep$hazard[grepl("observer:hz#1$", rep$id)], "n, m")
  expect_true(is.na(rep$hazard[grepl("reactiveVal:rv#1$", rep$id)]))
  expect_true("input" %in% rep$id)
})

test_that("a destroyed reactiveVal is not snapshotted", {
  server <- function(input, output, session) {
    gone <- reactiveVal(1)
    kept <- reactiveVal(2)
  }
  s <- snapshot_session(server)
  attr(s$env$gone, ".impl", exact = TRUE)$destroy()
  snap <- s$.snapshotRegistry$snapshot()
  kept <- sub("^[^|]*\\|[^|]*\\|", "", names(snap$nodes))
  expect_setequal(kept, "reactiveVal:kept#1")
  expect_false(any(grepl("reactiveVal:gone#1$", snap$report$id)))
})

test_that("disableResume() on a reactiveValues excludes its values and reports why", {
  server <- function(input, output, session) {
    rv <- reactiveValues(a = 1)
    disableResume(rv)
  }
  snap <- snapshot_session(server)$.snapshotRegistry$snapshot()
  rep <- snap$report
  expect_identical(rep$reason[grepl("reactiveValues:rv#1$", rep$id)], "disableResume()")
  expect_false(any(grepl("reactiveValues:rv#1$", names(snap$nodes))))
})

test_that("the walk visits each conductor once per snapshot, so layered diamonds stay cheap", {
  layer <- function(prev) list(reactive(prev[[1]]() + prev[[2]]()), reactive(prev[[1]]() - prev[[2]]()))
  server <- function(input, output, session) {
    v <- reactiveVal(1)
    top <- Reduce(function(prev, i) layer(prev), 1:24, list(reactive(v()), reactive(input$x)))
    o <- observe({ top[[1]](); top[[2]]() })
  }
  s <- snapshot_session(server)
  s$setInputs(x = 1)
  # Without memoisation the walk takes 2^25 steps here.
  setTimeLimit(elapsed = 20, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  snap <- s$.snapshotRegistry$snapshot()
  setTimeLimit(elapsed = Inf)
  rec <- snap$nodes[[grep("observer:o#1$", names(snap$nodes))]]
  expect_true(rec$adoptable)
  expect_setequal(vapply(rec$sources, `[[`, character(1), "id"), c(grep("reactiveVal:v#1$", names(snap$nodes), value = TRUE), "input"))
})

test_that("a garbage-collected source is skipped by the walk and pruned from the registry", {
  server <- function(input, output, session) {
    holder <- new.env()
    holder$v <- reactiveVal(1, label = "v")
    w <- reactiveVal(2)
    o <- observe({ if (exists("v", holder)) holder$v(); w() })
  }
  s <- snapshot_session(server)
  s$flushReact()
  reg <- s$.snapshotRegistry
  rec <- record_for(s, "observer:o#1$")
  expect_length(rec$sources, 2)
  rm("v", envir = s$env$holder)
  gc()
  snap <- reg$snapshot()
  rec <- snap$nodes[[grep("observer:o#1$", names(snap$nodes))]]
  expect_true(rec$adoptable)
  expect_identical(sub("^[^|]*\\|[^|]*\\|", "", vapply(rec$sources, `[[`, character(1), "id")), "reactiveVal:w#1")
  expect_false(any(grepl("reactiveVal:v#1$", reg$ids())))
  expect_false(any(grepl("reactiveVal:v#1$", snap$report$id)))
})
