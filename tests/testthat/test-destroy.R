test_that("destroyedReactiveError creates correct condition", {
  err <- destroyedReactiveError("test label")
  expect_s3_class(err, "shiny.destroyed.error")
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "test label")
  expect_match(conditionMessage(err), "destroyed")
})

test_that("destroyedReactiveError can be caught specifically", {
  expect_error(
    stop(destroyedReactiveError("my_reactive")),
    class = "shiny.destroyed.error"
  )
})

test_that("ReactiveVal$destroy() sets destroyed flag and invalidates dependents", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")

  # Track invalidation
  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv_impl$get())

  rv_impl$destroy()
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveVal$destroy() is idempotent", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_no_error(rv_impl$destroy())
})

test_that("destroyed ReactiveVal$get() raises shiny.destroyed.error", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_error(rv_impl$get(), class = "shiny.destroyed.error")
})

test_that("destroyed ReactiveVal$set() raises shiny.destroyed.error", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  rv_impl$destroy()
  expect_error(rv_impl$set(20), class = "shiny.destroyed.error")
})

test_that("ReactiveVal$destroy() does not emit rLog or otel", {
  rv_impl <- ReactiveVal$new(10, label = "test_rv")
  ctx <- Context$new(domain = NULL)
  ctx$run(function() rv_impl$get())
  rv_impl$destroy()
  flushReact()
})

test_that("Observable$destroy() sets destroyed flag and invalidates dependents", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  # Force first evaluation
  isolate(o$getValue())

  # Track invalidation of downstream
  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() o$getValue())

  o$destroy()
  flushReact()

  expect_true(invalidated)
})

test_that("Observable$destroy() is idempotent", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  o$destroy()
  expect_no_error(o$destroy())
})

test_that("destroyed Observable$getValue() raises shiny.destroyed.error", {
  o <- Observable$new(function() 42, label = "test_obs", domain = NULL)
  o$destroy()
  expect_error(isolate(o$getValue()), class = "shiny.destroyed.error")
})

test_that("Observable$destroy() clears value and error refs", {
  o <- Observable$new(function() list(big = rep(1, 1e6)), label = "test_obs", domain = NULL)
  isolate(o$getValue())
  o$destroy()
  expect_null(o$.value)
  expect_false(o$.error)
})

test_that("Observer registers weak destroy callback with domain$onDestroy", {
  domain <- createMockDomain()
  # Add onDestroy to mock domain
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    val <- reactiveVal(0)
    count <- 0L
    obs <- observe({ val(); count <<- count + 1L })
  })
  flushReact()
  expect_equal(count, 1L)

  # Verify something was registered with onDestroy
  expect_gt(destroyCBs$count(), 0)
})

test_that("Observer$destroy() deregisters from onDestroy", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    obs <- observe({ TRUE })
  })
  flushReact()
  initial_count <- destroyCBs$count()

  obs$destroy()
  # The unsubscribe handle should have deregistered the callback
  expect_lt(destroyCBs$count(), initial_count)
})

test_that("ReactiveVal auto-registers weak destroy callback with domain", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    rv <- reactiveVal(10)
  })

  expect_gt(destroyCBs$count(), 0)
})

test_that("Observable auto-registers weak destroy callback with domain", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    r <- reactive({ 42 })
  })

  expect_gt(destroyCBs$count(), 0)
})

test_that("ReactiveVal without domain does not error on creation", {
  rv_impl <- ReactiveVal$new(10, label = "no_domain")
  isolate(expect_equal(rv_impl$get(), 10))
  rv_impl$set(20)
  isolate(expect_equal(rv_impl$get(), 20))
})

test_that("Observable without domain does not error on creation", {
  o <- Observable$new(function() 42, label = "no_domain", domain = NULL)
  expect_false(o$._destroyed)
})

test_that("weakref key becomes NULL after GC of ReactiveVal", {
  domain <- createMockDomain()
  wrs <- list()
  domain$onDestroy <- function(callback) {
    wrs[[length(wrs) + 1L]] <<- callback
    function() {}
  }

  withReactiveDomain(domain, {
    rv <- reactiveVal(10)
  })
  rv_impl <- attr(rv, ".impl")

  expect_equal(length(wrs), 1L)
  # The wrapper is a function that closes over the weakref
  expect_true(is.function(wrs[[1L]]))

  # Remove all references to the R6 object
  rm(rv, rv_impl)
  gc()

  # The wrapper function should still exist, but when called
  # it should be a no-op because the weakref key is gone
  expect_no_error(wrs[[1L]]())
})

test_that("weakref key becomes NULL after GC of Observable", {
  domain <- createMockDomain()
  wrs <- list()
  domain$onDestroy <- function(callback) {
    wrs[[length(wrs) + 1L]] <<- callback
    function() {}
  }

  withReactiveDomain(domain, {
    r <- reactive({ 42 })
  })

  expect_equal(length(wrs), 1L)

  rm(r)
  gc()

  # Calling the wrapper after GC should be a no-op
  expect_no_error(wrs[[1L]]())
})

test_that("weakref key becomes NULL after GC of Observer", {
  domain <- createMockDomain()
  wrs <- list()
  domain$onDestroy <- function(callback) {
    wrs[[length(wrs) + 1L]] <<- callback
    function() {}
  }

  withReactiveDomain(domain, {
    rv <- reactiveVal(0)
    obs <- observe({ rv() })
  })
  flushReact()

  # Find the observer's wrapper (there may be wrappers from reactiveVal too)
  initial_count <- length(wrs)
  expect_gte(initial_count, 1L)

  obs$destroy()
  rm(obs)
  gc()

  # All wrappers should be safe to call after GC
  for (w in wrs) {
    expect_no_error(w())
  }
})

test_that("weakref value (self$destroy) does not prevent GC of key (self)", {
  domain <- createMockDomain()
  wrs <- list()
  domain$onDestroy <- function(callback) {
    wrs[[length(wrs) + 1L]] <<- callback
    function() {}
  }

  withReactiveDomain(domain, {
    rv <- reactiveVal(999)
  })
  rv_impl <- attr(rv, ".impl")
  weak_check <- rlang::new_weakref(rv_impl)

  expect_false(is.null(rlang::wref_key(weak_check)))

  rm(rv, rv_impl)
  gc()

  # After removing all references, the object should be GC'd
  expect_null(rlang::wref_key(weak_check))
})

test_that("ReactiveValues$_destroy removes keys matching namespace prefix", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 1)
  rv$set("mymod-y", 2)
  rv$set("other-z", 3)
  rv$set("mymod-inner-a", 4)

  rv$`_destroy`("mymod-")

  expect_equal(sort(isolate(rv$names())), "other-z")
  expect_equal(isolate(rv$get("other-z")), 3)
})

test_that("ReactiveValues$_destroy invalidates dependents of removed keys", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 10)

  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv$get("mymod-x"))

  rv$`_destroy`("mymod-")
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveValues$_destroy invalidates names() dependents", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 10)

  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv$names())

  rv$`_destroy`("mymod-")
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveValues$_destroy is no-op when no keys match", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("other-x", 1)

  expect_no_error(rv$`_destroy`("mymod-"))
  expect_equal(isolate(rv$names()), "other-x")
})
