# Tests for label methods used in otel-bind.R
test_that("ospan_label_reactive generates correct labels", {
  # Create mock reactive with observable attribute
  x_reactive <- reactive({ 42 })

  # Create mock observable with label
  x_observe <- observe({ 42 })

  # Test without domain
  result <- ospan_label_reactive(x_reactive, domain = MockShinySession$new())
  expect_equal(result, "reactive mock-session:x_reactive")

  # Test with cache class
  x_reactive_cache <- bindCache(x_reactive, {"cacheKey"})
  result <- ospan_label_reactive(x_reactive_cache, domain = NULL)
  expect_equal(result, "reactive cache x_reactive_cache")

  # Test with event class
  x_reactive_event <- bindEvent(x_reactive, {"eventKey"})
  result <- ospan_label_reactive(x_reactive_event, domain = NULL)
  expect_equal(result, "reactive event x_reactive_event")

  # x_reactive_both <- bindCache(bindEvent(x_reactive, {"eventKey"}), {"cacheKey"})
  # result <- ospan_label_reactive(x_reactive_both, domain = NULL)
  # expect_equal(result, "reactive event cache x_reactive_both")

  x_reactive_both2 <- bindEvent(bindCache(x_reactive, {"cacheKey"}), {"eventKey"})
  result <- ospan_label_reactive(x_reactive_both2, domain = NULL)
  expect_equal(result, "reactive cache event x_reactive_both2")
})

test_that("reactive bindCache labels are created", {
  x_reactive <- reactive({ 42 })
  x_reactive_cache <- bindCache(x_reactive, {"cacheKey"})

  expect_equal(
    as.character(attr(x_reactive_cache, "observable")$.label),
    "x_reactive_cache"
  )

  f_cache <- function() {
    bindCache(x_reactive, {"cacheKey"})
  }
  x_reactive_cache <- f_cache()
  expect_equal(
    as.character(attr(x_reactive_cache, "observable")$.label),
    "cachedReactive(x_reactive)"
  )
  expect_equal(
    ospan_label_reactive(x_reactive_cache, domain = NULL),
    "reactive cache <anonymous>"
  )
})

test_that("ospan_label_reactive with pre-defined label", {
  x_reactive <- reactive({ 42 }, label = "counter")

  result <- ospan_label_reactive(x_reactive, domain = MockShinySession$new())
  expect_equal(result, "reactive mock-session:counter")

  result <- ospan_label_reactive(x_reactive, domain = NULL)
  expect_equal(result, "reactive counter")
})

test_that("observer labels are preserved", {
  x_observe <- observe({ 42 }, label = "my_observer")
  expect_equal(x_observe$.label, "my_observer")
  expect_equal(ospan_label_observer(x_observe, domain = NULL), "observe my_observer")

  x_observe <- observe({ 42 })
  expect_equal(x_observe$.label, "x_observe")
  expect_equal(ospan_label_observer(x_observe, domain = NULL), "observe x_observe")

  f <- function() {
    observe({ 42 })
  }

  x_observe <- f()
  expect_equal(x_observe$.label, as_default_label("observe({\n    42\n})"))
  expect_equal(ospan_label_observer(x_observe, domain = NULL), "observe <anonymous>")
})

test_that("ospan_label_observer generates correct labels", {
  x_observe <- observe({ 42 }, label = "test_observer" )

  result <- ospan_label_observer(x_observe, domain = MockShinySession$new())
  expect_equal(result, "observe mock-session:test_observer")
  result <- ospan_label_observer(x_observe, domain = NULL)
  expect_equal(result, "observe test_observer")

  x_observe_event <- bindEvent(x_observe, {"eventKey"})
  result <- ospan_label_observer(x_observe_event, domain = NULL)
  expect_equal(result, "observe event x_observe_event")
})

test_that("ospan_label_observer handles module namespacing", {
  x_observe <- observe({ 42 }, label = "clicks" )
  result <- ospan_label_observer(x_observe, domain = MockShinySession$new())
  expect_equal(result, "observe mock-session:clicks")
})

test_that("ospan_label_render_function generates correct labels", {
  x_render <- renderText({ "Hello" })
  mock_domain <- MockShinySession$new()

  testthat::local_mocked_bindings(
    getCurrentOutputInfo = function(session) {
      list(name = "plot1")
    }
  )

  result <- ospan_label_render_function(x_render, domain = NULL)
  expect_equal(result, "output plot1")

  result <- ospan_label_render_function(x_render, domain = mock_domain)
  expect_equal(result, "output mock-session:plot1")

  x_render_event <- bindEvent(x_render, {"eventKey"})
  result <- ospan_label_render_function(x_render_event, domain = mock_domain)
  expect_equal(result, "output event mock-session:plot1")

  x_render_cache <- bindCache(x_render, {"cacheKey"})
  result <- ospan_label_render_function(x_render_cache, domain = mock_domain)
  expect_equal(result, "output cache mock-session:plot1")

  x_render_both <- bindEvent(bindCache(x_render, {"cacheKey"}), {"eventKey"})
  result <- ospan_label_render_function(x_render_both, domain = mock_domain)
  expect_equal(result, "output cache event mock-session:plot1")
})


test_that("ospan_label_render_function handles cache and event classes", {
  testthat::local_mocked_bindings(
    getCurrentOutputInfo = function(session) {
      list(name = "table1")
    }
  )

  x_render <- renderText({ "Hello" })
  x_render_event <- bindEvent(x_render, {"eventKey"})
  x_render_cache <- bindCache(x_render, {"cacheKey"})
  x_render_both <- bindEvent(bindCache(x_render, {"cacheKey"}), {"eventKey"})
  mock_domain <- MockShinySession$new()

  result <- ospan_label_render_function(x_render, domain = NULL)
  expect_equal(result, "output table1")

  result <- ospan_label_render_function(x_render, domain = mock_domain)
  expect_equal(result, "output mock-session:table1")

  result <- ospan_label_render_function(x_render_event, domain = mock_domain)
  expect_equal(result, "output event mock-session:table1")

  result <- ospan_label_render_function(x_render_cache, domain = mock_domain)
  expect_equal(result, "output cache mock-session:table1")

  result <- ospan_label_render_function(x_render_both, domain = mock_domain)
  expect_equal(result, "output cache event mock-session:table1")
})

test_that("otel_label_upgrade handles anonymous labels", {
  # Test default labels with parentheses get converted to <anonymous>
  result <- otel_label_upgrade(as_default_label("observe({})"), domain = NULL)
  expect_equal(result, "<anonymous>")

  result <- otel_label_upgrade(as_default_label("eventReactive(input$btn, {})"), domain = NULL)
  expect_equal(result, "<anonymous>")

  # Test regular labels are kept as-is
  result <- otel_label_upgrade(as_default_label("my_observer"), domain = NULL)
  expect_equal(as.character(result), "my_observer")
  result <- otel_label_upgrade("my_observer", domain = NULL)
  expect_equal(result, "my_observer")
})
