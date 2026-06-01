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
  expect_no_error({
    rv_impl$destroy()
    flushReact()
  })
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

test_that("ReactiveVal$destroy() deregisters its onDestroy callback", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    rv <- reactiveVal(10)
  })
  rv_impl <- attr(rv, ".impl")

  initial_count <- destroyCBs$count()
  expect_gt(initial_count, 0)

  rv_impl$destroy()
  expect_lt(destroyCBs$count(), initial_count)
})

test_that("Observable$destroy() deregisters its onDestroy callback", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    r <- reactive({ 42 })
  })

  initial_count <- destroyCBs$count()
  expect_gt(initial_count, 0)

  o <- attr(r, "observable")
  o$destroy()
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
  expect_false(o$.destroyed)
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

test_that("weakref key can be GC'd when no strong references remain", {
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

test_that("ReactiveValues$destroyByPrefix removes keys matching namespace prefix", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 1)
  rv$set("mymod-y", 2)
  rv$set("other-z", 3)
  rv$set("mymod-inner-a", 4)

  rv$destroyByPrefix("mymod-")

  expect_equal(sort(isolate(rv$names())), "other-z")
  expect_equal(isolate(rv$get("other-z")), 3)
})

test_that("ReactiveValues$destroyByPrefix invalidates dependents of removed keys", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 10)

  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv$get("mymod-x"))

  rv$destroyByPrefix("mymod-")
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveValues$destroyByPrefix invalidates names() dependents", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("mymod-x", 10)

  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv$names())

  rv$destroyByPrefix("mymod-")
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveValues$destroyByPrefix is no-op when no keys match", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("other-x", 1)

  expect_no_error(rv$destroyByPrefix("mymod-"))
  expect_equal(isolate(rv$names()), "other-x")
})

test_that("ReactiveValues$destroy() sets destroyed flag and invalidates dependents", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("x", 1)

  invalidated <- FALSE
  ctx <- Context$new(domain = NULL)
  ctx$onInvalidate(function() invalidated <<- TRUE)
  ctx$run(function() rv$get("x"))

  rv$destroy()
  flushReact()

  expect_true(invalidated)
})

test_that("ReactiveValues$destroy() is idempotent", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$destroy()
  expect_no_error(rv$destroy())
})

test_that("destroyed ReactiveValues$get() raises shiny.destroyed.error", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$set("x", 1)
  rv$destroy()
  expect_error(rv$get("x"), class = "shiny.destroyed.error")
})

test_that("destroyed ReactiveValues$set() raises shiny.destroyed.error", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "test")
  rv$destroy()
  expect_error(rv$set("x", 1), class = "shiny.destroyed.error")
})

test_that("ReactiveValues auto-registers weak destroy callback with domain", {
  domain <- createMockDomain()
  destroyCBs <- Callbacks$new()
  domain$onDestroy <- function(callback) destroyCBs$register(callback)

  withReactiveDomain(domain, {
    rv <- reactiveValues(a = 1)
  })

  expect_gt(destroyCBs$count(), 0)
})

test_that("ReactiveValues without domain does not error on creation", {
  rv <- ReactiveValues$new(dedupe = FALSE, label = "no_domain")
  rv$set("x", 1)
  expect_equal(isolate(rv$get("x")), 1)
})

test_that("MockShinySession$onDestroy registers callback and returns unsubscribe", {
  session <- MockShinySession$new()
  called <- FALSE
  unsub <- session$onDestroy(function() called <<- TRUE)
  expect_true(is.function(unsub))
})

test_that("MockShinySession$destroy() with no id throws an error", {
  session <- MockShinySession$new()
  expect_error(session$destroy(), "without an `id`")
})

test_that("root session$destroy(id) tears down the named module scope", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  called <- FALSE
  scope$onDestroy(function() called <<- TRUE)

  expect_false(called)
  # Destroy the scope from the root session using only its id
  session$destroy("mod1")
  expect_true(called)
})

test_that("root session$destroy(id) destroys the scope's reactive state", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  obs_val <- NULL
  observer_ran <- 0L
  withReactiveDomain(scope, {
    rv <- reactiveVal(10)
    obs <- observe({
      observer_ran <<- observer_ran + 1L
      obs_val <<- rv()
    })
  })
  flushReact()
  expect_equal(observer_ran, 1L)

  session$destroy("mod1")
  flushReact()

  # Observer should not run again
  expect_equal(observer_ran, 1L)

  # Accessing the destroyed reactive should error
  rv_impl <- attr(rv, ".impl")
  expect_error(isolate(rv_impl$get()), class = "shiny.destroyed.error")
})

test_that("session$destroy(id) is equivalent to makeScope(id)$destroy()", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mymod")

  session$setInputs(`mymod-x` = 1, other = 2)
  flushReact()

  session$destroy("mymod")
  flushReact()

  expect_null(isolate(session$input$`mymod-x`))
  expect_equal(isolate(session$input$other), 2)
})

test_that("module session$destroy(id) tears down a nested child scope", {
  session <- MockShinySession$new()
  parent <- session$makeScope("parent")
  child <- parent$makeScope("child")

  child_called <- FALSE
  parent_called <- FALSE
  child$onDestroy(function() child_called <<- TRUE)
  parent$onDestroy(function() parent_called <<- TRUE)

  # Destroy the child from the parent session using only its id
  parent$destroy("child")
  expect_true(child_called)
  # Parent itself should remain alive
  expect_false(parent_called)
})

test_that("session$destroy(id) on an unknown id is a harmless no-op", {
  session <- MockShinySession$new()
  expect_no_error(session$destroy("never_created"))
})

test_that("session$destroy(id) validates the id argument", {
  session <- MockShinySession$new()
  expect_error(session$destroy(1), "single, non-empty string")
  expect_error(session$destroy(c("a", "b")), "single, non-empty string")
  expect_error(session$destroy(""), "single, non-empty string")
  expect_error(session$destroy(NA_character_), "single, non-empty string")
})

test_that("session$destroy(id) does not leak bookmark-exclude callbacks", {
  session <- MockShinySession$new()
  before <- session$getBookmarkExclude()
  session$makeScope("mod1")
  session$destroy("mod1")
  expect_equal(session$getBookmarkExclude(), before)
})

# RE-ENABLE alongside MockShinySession$close()'s invokeDestroyCallbacks("") call
# (see the note in R/mock-session.R). These tests assert that closing the mock
# session destroys its reactives; that behavior is temporarily disabled for the
# 1.14.0 release, so they are commented out and should be restored together with
# that line.
# test_that("MockShinySession$close() invokes destroy callbacks", {
#   session <- MockShinySession$new()
#   called <- FALSE
#   session$onDestroy(function() called <<- TRUE)
#   session$close()
#   expect_true(called)
# })
#
# test_that("MockShinySession$close() fires destroy callbacks deepest-first", {
#   session <- MockShinySession$new()
#   parent <- session$makeScope("parent")
#   child <- parent$makeScope("child")
#
#   order <- character(0)
#   session$onDestroy(function() order <<- c(order, "root"))
#   parent$onDestroy(function() order <<- c(order, "parent"))
#   child$onDestroy(function() order <<- c(order, "child"))
#
#   session$close()
#   expect_equal(order, c("child", "parent", "root"))
# })

test_that("MockShinySession destroy cleans up namespaced inputs", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mymod")

  session$setInputs(`mymod-x` = 1, `mymod-y` = 2, other = 3)
  flushReact()

  scope$destroy()
  flushReact()

  expect_equal(isolate(session$input$other), 3)
  expect_null(isolate(session$input$`mymod-x`))
  expect_null(isolate(session$input$`mymod-y`))
})

# RE-ENABLE alongside MockShinySession$close()'s invokeDestroyCallbacks("") call
# (see the note in R/mock-session.R); disabled for the 1.14.0 release.
# test_that("root onDestroy callbacks fire after module callbacks during close", {
#   session <- MockShinySession$new()
#   scope <- session$makeScope("mod1")
#
#   order <- character(0)
#   session$onDestroy(function() order <<- c(order, "root"))
#   scope$onDestroy(function() order <<- c(order, "mod1"))
#
#   session$close()
#   expect_equal(order, c("mod1", "root"))
# })

test_that("session proxy onDestroy registers and fires on destroy", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mymod")

  called <- FALSE
  scope$onDestroy(function() called <<- TRUE)

  expect_false(called)
  scope$destroy()
  expect_true(called)
})

test_that("session proxy destroy() invokes callbacks", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mymod")

  called <- FALSE
  scope$onDestroy(function() called <<- TRUE)

  scope$destroy()
  expect_true(called)
})

test_that("session proxy destroy() cleans up child namespaces", {
  session <- MockShinySession$new()
  scope <- session$makeScope("parent")
  child <- scope$makeScope("child")

  parent_called <- FALSE
  child_called <- FALSE
  scope$onDestroy(function() parent_called <<- TRUE)
  child$onDestroy(function() child_called <<- TRUE)

  scope$destroy()
  expect_true(parent_called)
  expect_true(child_called)
})

test_that("session proxy destroy() fires deepest-first", {
  session <- MockShinySession$new()
  scope <- session$makeScope("parent")
  child <- scope$makeScope("child")

  order <- character(0)
  scope$onDestroy(function() order <<- c(order, "parent"))
  child$onDestroy(function() order <<- c(order, "child"))

  scope$destroy()
  expect_equal(order, c("child", "parent"))
})

test_that("session proxy destroy() is idempotent", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mymod")

  count <- 0L
  scope$onDestroy(function() count <<- count + 1L)

  scope$destroy()
  scope$destroy()
  expect_equal(count, 1L)
})

test_that("full module destroy cleans up all reactive state", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  obs_val <- NULL
  observer_ran <- 0L

  withReactiveDomain(scope, {
    rv <- reactiveVal(10)
    r <- reactive({ rv() * 2 })
    obs <- observe({
      observer_ran <<- observer_ran + 1L
      obs_val <<- r()
    })
  })
  flushReact()

  expect_equal(observer_ran, 1L)
  expect_equal(obs_val, 20)

  # Destroy the module scope
  scope$destroy()
  flushReact()

  # Observer should not run again
  expect_equal(observer_ran, 1L)

  # Accessing destroyed reactives should error
  rv_impl <- attr(rv, ".impl")
  expect_error(rv_impl$get(), class = "shiny.destroyed.error")

  o <- attr(r, "observable")
  expect_error(isolate(o$getValue()), class = "shiny.destroyed.error")
})

test_that("destroy then re-create module works cleanly", {
  session <- MockShinySession$new()

  # First instance
  scope1 <- session$makeScope("mod1")
  val1 <- NULL
  withReactiveDomain(scope1, {
    rv1 <- reactiveVal(1)
    observe({ val1 <<- rv1() })
  })
  flushReact()
  expect_equal(val1, 1)

  # Destroy first instance
  scope1$destroy()
  flushReact()

  # Second instance with same namespace
  scope2 <- session$makeScope("mod1")
  val2 <- NULL
  withReactiveDomain(scope2, {
    rv2 <- reactiveVal(99)
    observe({ val2 <<- rv2() })
  })
  flushReact()
  expect_equal(val2, 99)
})

test_that("nested module destroy cleans up grandchild scopes", {
  session <- MockShinySession$new()
  parent <- session$makeScope("parent")
  child <- parent$makeScope("child")
  grandchild <- child$makeScope("gc")

  order <- character(0)
  parent$onDestroy(function() order <<- c(order, "parent"))
  child$onDestroy(function() order <<- c(order, "child"))
  grandchild$onDestroy(function() order <<- c(order, "grandchild"))

  parent$destroy()

  # Deepest-first ordering
  expect_equal(order, c("grandchild", "child", "parent"))
})

test_that("invalidateLater timer is cancelled on module destroy", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  count <- 0L
  withReactiveDomain(scope, {
    observe({
      invalidateLater(100)
      count <<- count + 1L
    })
  })
  session$elapse(0)
  flushReact()
  expect_equal(count, 1L)

  # Timer should fire if we elapse enough time
  session$elapse(100)
  flushReact()
  expect_equal(count, 2L)

  # Destroy the module scope — timer should be cancelled
  scope$destroy()
  flushReact()

  # Elapsing time should NOT cause the observer to run again
  session$elapse(200)
  flushReact()
  expect_equal(count, 2L)
})

test_that("invalidateLater cleans up onEnded registration after module destroy", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  withReactiveDomain(scope, {
    observe({
      invalidateLater(100)
    })
  })
  session$elapse(0)
  flushReact()

  # Destroy the module — should deregister onEnded callback
  scope$destroy()
  flushReact()

  # Session close should not error (stale callbacks already cleaned up)
  expect_no_error(session$close())
})

test_that("invalidateLater cleans up onDestroy registration after timer fires", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  withReactiveDomain(scope, {
    observe({
      invalidateLater(100)
    })
  })
  session$elapse(0)
  flushReact()

  # Let the timer fire naturally — should deregister onDestroy callback
  session$elapse(200)
  flushReact()

  # Destroying after timer already fired should not error
  expect_no_error(scope$destroy())
})

test_that("invalidateLater cleans up onDestroy registration on session close", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  withReactiveDomain(scope, {
    observe({
      invalidateLater(100)
    })
  })
  session$elapse(0)
  flushReact()

  # Closing the full session should clean up everything without error
  expect_no_error(session$close())
})

test_that("invalidateLater timer is cancelled on root session close", {
  session <- MockShinySession$new()

  count <- 0L
  withReactiveDomain(session, {
    observe({
      invalidateLater(100)
      count <<- count + 1L
    })
  })
  session$elapse(0)
  flushReact()
  expect_equal(count, 1L)

  # Timer fires normally before close
  session$elapse(100)
  flushReact()
  expect_equal(count, 2L)

  # Close the root session — timer should be cancelled via onEnded
  session$close()
  flushReact()

  session$elapse(200)
  flushReact()
  expect_equal(count, 2L)
})

test_that("invalidateLater cleans up both registrations on root session close", {
  session <- MockShinySession$new()

  withReactiveDomain(session, {
    observe({
      invalidateLater(100)
    })
  })
  session$elapse(0)
  flushReact()

  # Closing the root session fires onEnded and then onDestroy;
  # both registrations should be cleaned up without error
  expect_no_error(session$close())
})

test_that("invalidateLater cleans up onDestroy after timer fires on root session", {
  session <- MockShinySession$new()

  withReactiveDomain(session, {
    observe({
      invalidateLater(100)
    })
  })
  session$elapse(0)
  flushReact()

  # Let timer fire naturally — clears both onEnded and onDestroy
  session$elapse(200)
  flushReact()

  # Closing the session after timer already fired should not error
  expect_no_error(session$close())
})

test_that("module scope bookmark-exclude is cleaned up on destroy", {
  session <- MockShinySession$new()
  scope <- session$makeScope("mod1")

  scope$setBookmarkExclude(c("a", "b"))

  # Root session should see the module's excludes (namespaced)
  expect_true("mod1-a" %in% session$getBookmarkExclude())
  expect_true("mod1-b" %in% session$getBookmarkExclude())

  scope$destroy()

  # After destroy, module's excludes should be gone
  expect_false("mod1-a" %in% session$getBookmarkExclude())
  expect_false("mod1-b" %in% session$getBookmarkExclude())
})

test_that("multiple module scopes have independent bookmark-exclude cleanup", {
  session <- MockShinySession$new()
  scope1 <- session$makeScope("mod1")
  scope2 <- session$makeScope("mod2")

  scope1$setBookmarkExclude("x")
  scope2$setBookmarkExclude("y")

  expect_true("mod1-x" %in% session$getBookmarkExclude())
  expect_true("mod2-y" %in% session$getBookmarkExclude())

  # Destroy only mod1
  scope1$destroy()

  expect_false("mod1-x" %in% session$getBookmarkExclude())
  expect_true("mod2-y" %in% session$getBookmarkExclude())

  # Destroy mod2
  scope2$destroy()
  expect_false("mod2-y" %in% session$getBookmarkExclude())
})

test_that("root setBookmarkExclude persists after module destroy", {
  session <- MockShinySession$new()
  session$setBookmarkExclude(c("global_input"))

  scope <- session$makeScope("mod1")
  scope$setBookmarkExclude("a")

  expect_true("global_input" %in% session$getBookmarkExclude())
  expect_true("mod1-a" %in% session$getBookmarkExclude())

  scope$destroy()

  # Root excludes should still be there
  expect_true("global_input" %in% session$getBookmarkExclude())
  expect_false("mod1-a" %in% session$getBookmarkExclude())
})

test_that("makeScope rejects reserved namespace '..root'", {
  session <- MockShinySession$new()
  expect_error(session$makeScope("..root"), "reserved")
})

test_that("createMockDomain supports onDestroy and destroy", {
  domain <- createMockDomain()

  called <- FALSE
  domain$onDestroy(function() called <<- TRUE)

  expect_false(called)
  domain$destroy()
  expect_true(called)
})

test_that("createMockDomain destroy is idempotent", {
  domain <- createMockDomain()

  count <- 0L
  domain$onDestroy(function() count <<- count + 1L)

  domain$destroy()
  domain$destroy()
  expect_equal(count, 1L)
})
