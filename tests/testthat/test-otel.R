test_that("otel_bind_is_enabled works with valid bind levels", {
  # Test with default "all" option
  expect_true(otel_bind_is_enabled("none"))
  expect_true(otel_bind_is_enabled("session"))
  expect_true(otel_bind_is_enabled("reactive_update"))
  expect_true(otel_bind_is_enabled("reactivity"))
  expect_true(otel_bind_is_enabled("all"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'none' option", {
  # With "none" option, nothing should be enabled
  expect_false(otel_bind_is_enabled("session", "none"))
  expect_false(otel_bind_is_enabled("reactive_update", "none"))
  expect_false(otel_bind_is_enabled("reactivity", "none"))
  expect_false(otel_bind_is_enabled("all", "none"))
  expect_true(otel_bind_is_enabled("none", "none"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'session' option", {
  # With "session" option, only "none" and "session" should be enabled
  expect_true(otel_bind_is_enabled("none", "session"))
  expect_true(otel_bind_is_enabled("session", "session"))
  expect_false(otel_bind_is_enabled("reactive_update", "session"))
  expect_false(otel_bind_is_enabled("reactivity", "session"))
  expect_false(otel_bind_is_enabled("all", "session"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'reactive_update' option", {
  # With "reactive_update" option, "none", "session", and "reactive_update" should be enabled
  expect_true(otel_bind_is_enabled("none", "reactive_update"))
  expect_true(otel_bind_is_enabled("session", "reactive_update"))
  expect_true(otel_bind_is_enabled("reactive_update", "reactive_update"))
  expect_false(otel_bind_is_enabled("reactivity", "reactive_update"))
  expect_false(otel_bind_is_enabled("all", "reactive_update"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'reactivity' option", {
  # With "reactivity" option, all except "all" should be enabled
  expect_true(otel_bind_is_enabled("none", "reactivity"))
  expect_true(otel_bind_is_enabled("session", "reactivity"))
  expect_true(otel_bind_is_enabled("reactive_update", "reactivity"))
  expect_true(otel_bind_is_enabled("reactivity", "reactivity"))
  expect_false(otel_bind_is_enabled("all", "reactivity"))
})

test_that("otel_bind_is_enabled respects hierarchy with 'all' option", {
  # With "all" option (default), everything should be enabled
  expect_true(otel_bind_is_enabled("none", "all"))
  expect_true(otel_bind_is_enabled("session", "all"))
  expect_true(otel_bind_is_enabled("reactive_update", "all"))
  expect_true(otel_bind_is_enabled("reactivity", "all"))
  expect_true(otel_bind_is_enabled("all", "all"))
})

test_that("otel_bind_is_enabled uses shiny.otel.bind option", {
  # Test that option is respected
  withr::with_options(
    list(shiny.otel.bind = "session"),
    {
      expect_true(otel_bind_is_enabled("none"))
      expect_true(otel_bind_is_enabled("session"))
      expect_false(otel_bind_is_enabled("reactive_update"))
    }
  )

  withr::with_options(
    list(shiny.otel.bind = "reactivity"),
    {
      expect_true(otel_bind_is_enabled("reactive_update"))
      expect_true(otel_bind_is_enabled("reactivity"))
      expect_false(otel_bind_is_enabled("all"))
    }
  )
})

test_that("otel_bind_is_enabled falls back to SHINY_OTEL_BIND env var", {
  # Remove option to test env var fallback
  withr::local_options(list(shiny.otel.bind = NULL))

  # Test env var is respected
  withr::local_envvar(list(SHINY_OTEL_BIND = "session"))
  expect_true(otel_bind_is_enabled("none"))
  expect_true(otel_bind_is_enabled("session"))
  expect_false(otel_bind_is_enabled("reactive_update"))

  withr::local_envvar(list(SHINY_OTEL_BIND = "none"))
  expect_true(otel_bind_is_enabled("none"))
  expect_false(otel_bind_is_enabled("session"))
})

test_that("otel_bind_is_enabled option takes precedence over env var", {
  # Set conflicting option and env var
  withr::local_options(shiny.otel.bind = "session")
  withr::local_envvar(SHINY_OTEL_BIND = "all")

  # Option should take precedence
  expect_true(otel_bind_is_enabled("session"))
  expect_false(otel_bind_is_enabled("reactive_update"))
})

test_that("otel_bind_is_enabled defaults to 'all' when no option or env var", {
  # Remove both option and env var
  withr::local_options(list(shiny.otel.bind = NULL))
  withr::local_envvar(list(SHINY_OTEL_BIND = NA))

  # Should default to "all"
  expect_true(otel_bind_is_enabled("all"))
  expect_true(otel_bind_is_enabled("reactivity"))
  expect_true(otel_bind_is_enabled("none"))
})

# Tests for as_otel_bind()
test_that("as_otel_bind validates and returns valid bind levels", {
  expect_equal(as_otel_bind("none"), "none")
  expect_equal(as_otel_bind("session"), "session")
  expect_equal(as_otel_bind("reactive_update"), "reactive_update")
  expect_equal(as_otel_bind("reactivity"), "reactivity")
  expect_equal(as_otel_bind("all"), "all")
})

test_that("as_otel_bind uses default value", {
  expect_equal(as_otel_bind(), "all")
})

test_that("as_otel_bind errors on invalid input types", {
  expect_error(as_otel_bind(123), "`bind` must be a character vector.")
  expect_error(as_otel_bind(NULL), "`bind` must be a character vector.")
  expect_error(as_otel_bind(TRUE), "`bind` must be a character vector.")
  expect_error(as_otel_bind(list("all")), "`bind` must be a character vector.")
})

test_that("as_otel_bind errors on invalid bind levels", {
  expect_error(as_otel_bind("invalid"), "'arg' should be one of")
  expect_error(as_otel_bind("unknown"), "'arg' should be one of")
  expect_error(as_otel_bind(""), "'arg' should be one of")
})

test_that("as_otel_bind errors on multiple values", {
  # match.arg with several.ok = FALSE should error on multiple values
  expect_error(as_otel_bind(c("all", "none")), "'arg' must be of length 1")
  expect_error(as_otel_bind(c("session", "reactivity")), "'arg' must be of length 1")
})

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
