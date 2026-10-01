# Edges are recorded only for nodes tracked by a session's registry, which
# exists only with the feature on; the suite also runs with
# SHINY_RESUME=FALSE.
withr::local_envvar(SHINY_RESUME = "TRUE")

owner_of <- function(dep) rlang::wref_key(dep$.snapshotOwner$ref)

test_that("a context records the Dependents it registered with, in order", {
  s <- MockShinySession$new()
  withReactiveDomain(s, {
    a <- reactiveVal(1)
    b <- reactiveValues(x = 1)
    r <- reactive(a() + b$x)
    o <- observe({ r(); b$x })
  })
  s$flushReact()

  up <- o$.ctx$.snapshot$upstream
  expect_length(up, 2)
  expect_identical(owner_of(up[[1]]), attr(r, "observable", exact = TRUE))
  expect_identical(up[[1]]$.snapshotOwner$part, "value")
  expect_identical(owner_of(up[[2]]), .subset2(b, "impl"))
  expect_identical(up[[2]]$.snapshotOwner$part, "key")
  expect_identical(up[[2]]$.snapshotOwner$key, "x")

  rctx <- attr(r, "observable", exact = TRUE)$.ctx
  expect_length(rctx$.snapshot$upstream, 2)
  expect_identical(owner_of(rctx$.snapshot$upstream[[1]]), attr(a, ".impl", exact = TRUE))
  expect_true(o$.ctx$.snapshot$adoptable)
  s$close()
})

test_that("names() and reactiveValuesToList() register their access kind", {
  s <- MockShinySession$new()
  withReactiveDomain(s, {
    b <- reactiveValues(x = 1, .hidden = 2)
    o <- observe({ names(b); reactiveValuesToList(b); reactiveValuesToList(b, all.names = TRUE) })
  })
  s$flushReact()
  parts <- vapply(o$.ctx$.snapshot$upstream, function(d) d$.snapshotOwner$part, character(1))
  expect_setequal(parts, c("names", "asList", "asListAll"))
  s$close()
})

test_that("invalidateLater() and reactiveTimer() mark the context not adoptable", {
  session <- MockShinySession$new()
  o1 <- observe({ invalidateLater(1000, session) }, domain = session)
  tm <- reactiveTimer(1000, session)
  o2 <- observe({ tm() }, domain = session)
  session$flushReact()
  expect_false(o1$.ctx$.snapshot$adoptable)
  expect_false(o2$.ctx$.snapshot$adoptable)
  session$close()
})

test_that("an untracked reactive marks the context not adoptable", {
  s <- MockShinySession$new()
  d <- Dependents$new()
  global <- reactiveVal(1)
  o1 <- observe({ d$register() }, domain = s)
  o2 <- observe({ global() }, domain = s)
  s$flushReact()
  for (o in list(o1, o2)) {
    expect_false(o$.ctx$.snapshot$adoptable)
    expect_match(o$.ctx$.snapshot$reason, "does not track")
    expect_length(o$.ctx$.snapshot$upstream, 0)
  }
  s$close()
})

test_that("a registered context is only recorded once per Dependents", {
  s <- MockShinySession$new()
  withReactiveDomain(s, {
    a <- reactiveVal(1)
    o <- observe({ a(); a(); a() })
  })
  s$flushReact()
  expect_length(o$.ctx$.snapshot$upstream, 1)
  s$close()
})

test_that("a context is unsettled while the run's promise is pending, and keeps collecting edges", {
  s <- MockShinySession$new()
  resolve_p <- NULL
  p <- promises::promise(function(resolve, reject) resolve_p <<- resolve)
  withReactiveDomain(s, {
    a <- reactiveVal(1)
    b <- reactiveVal(2)
    o <- observe({ a(); p$then(function(x) b()) })
  })
  s$flushReact()
  ctx <- o$.ctx
  expect_false(ctx$.snapshot$settled)
  expect_length(ctx$.snapshot$upstream, 1)
  resolve_p(NULL)
  for (i in 1:3) later::run_now()
  expect_true(ctx$.snapshot$settled)
  expect_length(ctx$.snapshot$upstream, 2)

  sync <- observe({ a() }, domain = s)
  s$flushReact()
  expect_true(sync$.ctx$.snapshot$settled)

  # A reactive whose value is a promise is unsettled until that promise settles.
  resolve_q <- NULL
  q <- promises::promise(function(resolve, reject) resolve_q <<- resolve)
  withReactiveDomain(s, {
    r <- reactive({ a(); q$then(function(x) b()) })
    reader <- observe({ r() })
  })
  s$flushReact()
  rctx <- attr(r, "observable", exact = TRUE)$.ctx
  expect_false(rctx$.snapshot$settled)
  resolve_q(NULL)
  for (i in 1:3) later::run_now()
  expect_true(rctx$.snapshot$settled)
  expect_length(rctx$.snapshot$upstream, 2)
  s$close()
})

test_that("a context is unsettled during its run, and settled after a run that errors", {
  s <- MockShinySession$new()
  during <- list()
  withReactiveDomain(s, {
    a <- reactiveVal(1)
    o <- observe({ a(); during$o <<- getCurrentContext()$.snapshot$settled })
    r <- reactive({ a(); during$r <<- getCurrentContext()$.snapshot$settled; stop("boom") })
    reader <- observe({ try(r(), silent = TRUE) })
    failing <- observe({ a(); req(FALSE) })
  })
  s$flushReact()
  expect_false(during$o)
  expect_false(during$r)
  expect_true(o$.ctx$.snapshot$settled)
  expect_true(attr(r, "observable", exact = TRUE)$.ctx$.snapshot$settled)
  expect_true(failing$.ctx$.snapshot$settled)
  s$close()
})

test_that("isolate() returns the identical promise it was given", {
  p <- promises::promise_resolve(1)
  expect_identical(isolate(p), p)
})

test_that("without a registry, contexts record nothing and Dependents have no owner", {
  check <- function(domain) {
    withReactiveDomain(domain, {
      a <- reactiveVal(1)
      b <- reactiveValues(x = 1)
      r <- reactive(a() + b$x)
      o <- observe({ r(); b$x; names(b); invalidateLater(1000) })
    })
    flushReact()
    expect_null(o$.snapshot)
    expect_null(o$.ctx$.snapshot)
    robs <- attr(r, "observable", exact = TRUE)
    expect_null(robs$.snapshot)
    expect_null(robs$.ctx$.snapshot)
    expect_null(robs$.dependents$.snapshotOwner)
    aimpl <- attr(a, ".impl", exact = TRUE)
    expect_null(aimpl$.snapshot)
    expect_null(aimpl$.__enclos_env__$private$dependents$.snapshotOwner)
    impl <- .subset2(b, "impl")
    expect_null(impl$.snapshot)
    expect_null(impl$.namesDeps$.snapshotOwner)
    expect_null(impl$.dependents$get("x")$.snapshotOwner)
    o$destroy()
  }
  check(NULL)
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), {
    s <- MockShinySession$new()
    expect_null(s$.snapshotRegistry)
    check(s)
    s$close()
  })
})
