# Tests for otel-shiny.R functions

# Helper function to create a mock otel span
create_mock_otel_span <- function() {
  structure(
    list(name = "test_span"),
    class = "otel_span"
  )
}

# Helper function to create a mock tracer
create_mock_tracer <- function() {
  structure(
    list(name = "mock_tracer", is_enabled = function() TRUE),
    class = "otel_tracer"
  )
}

# Helper function to create a mock logger
create_mock_logger <- function() {
  structure(
    list(name = "mock_logger"),
    class = "otel_logger"
  )
}

test_that("otel_tracer_name constant is correct", {
  expect_equal(otel_tracer_name, "co.posit.r-package.shiny")
})

test_that("with_shiny_ospan_async calls with_ospan_async with correct parameters", {
  mock_tracer <- create_mock_tracer()
  with_ospan_async_called <- FALSE
  test_value <- "initial"

  with_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
    with_ospan_async = function(name, expr, ..., attributes = NULL, tracer = NULL) {
      with_ospan_async_called <<- TRUE
      expect_equal(name, "test_span")
      expect_equal(tracer, mock_tracer)
      expect_equal(attributes, list(key = "value"))
      force(expr)
    },
    {
      result <- with_shiny_ospan_async(
        "test_span",
        {
          test_value <- "modified"
          "result_value"
        },
        attributes = list(key = "value")
      )

      expect_true(with_ospan_async_called)
      expect_equal(result, "result_value")
      expect_equal(test_value, "modified")
    }
  )
})

test_that("create_shiny_ospan calls otel::start_span with correct parameters", {
  mock_tracer <- create_mock_tracer()
  mock_span <- create_mock_otel_span()
  start_span_called <- FALSE

  local_mocked_bindings(
    start_span = function(name, ..., tracer = NULL) {
      start_span_called <<- TRUE
      expect_equal(name, "test_span")
      expect_equal(tracer, mock_tracer)
      mock_span
    },
    .package = "otel"
  )

  with_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
    {
      result <- create_shiny_ospan("test_span", extra_param = "value")

      expect_true(start_span_called)
      expect_equal(result, mock_span)
    }
  )
})

test_that("is_ospan correctly identifies otel spans", {
  # Test with otel_span object
  otel_span <- create_mock_otel_span()
  expect_true(is_ospan(otel_span))

  # Test with non-otel objects
  expect_false(is_ospan("string"))
  expect_false(is_ospan(123))
  expect_false(is_ospan(list()))
  expect_false(is_ospan(NULL))

  # Test with object that has different class
  other_obj <- structure(list(), class = "other_class")
  expect_false(is_ospan(other_obj))
})

test_that("testthat__is_testing detects testing environment", {
  # Test when TESTTHAT env var is set to "true"
  withr::local_envvar(list(TESTTHAT = "true"))
  expect_true(testthat__is_testing())

  # Test when TESTTHAT env var is not set
  withr::local_envvar(list(TESTTHAT = NA))
  expect_false(testthat__is_testing())

  # Test when TESTTHAT env var is set to other values
  withr::local_envvar(list(TESTTHAT = "false"))
  expect_false(testthat__is_testing())

  withr::local_envvar(list(TESTTHAT = ""))
  expect_false(testthat__is_testing())
})

test_that("otel_log calls otel::log with correct parameters", {
  mock_logger <- create_mock_logger()
  log_called <- FALSE

  local_mocked_bindings(
    log = function(msg, ..., severity = NULL, logger = NULL) {
      log_called <<- TRUE
      expect_equal(msg, "test message")
      expect_equal(severity, "warn")
      expect_equal(logger, mock_logger)
    },
    .package = "otel"
  )

  with_mocked_bindings(
    get_ospan_logger = function() mock_logger,
    {
      otel_log("test message", severity = "warn")
      expect_true(log_called)
    }
  )
})

test_that("otel_log uses default severity and logger", {
  mock_logger <- create_mock_logger()
  log_called <- FALSE

  local_mocked_bindings(
    log = function(msg, ..., severity = NULL, logger = NULL) {
      log_called <<- TRUE
      expect_equal(msg, "default test")
      expect_equal(severity, "info")  # Default severity
      expect_equal(logger, mock_logger)  # Default logger
    },
    .package = "otel"
  )

  with_mocked_bindings(
    get_ospan_logger = function() mock_logger,
    {
      otel_log("default test")
      expect_true(log_called)
    }
  )
})

test_that("otel_is_tracing_enabled calls otel::is_tracing_enabled", {
  mock_tracer <- create_mock_tracer()
  is_tracing_called <- FALSE

  local_mocked_bindings(
    is_tracing_enabled = function(tracer) {
      is_tracing_called <<- TRUE
      expect_equal(tracer, mock_tracer)
      TRUE
    },
    .package = "otel"
  )

  with_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
    {
      result <- otel_is_tracing_enabled()
      expect_true(is_tracing_called)
      expect_true(result)
    }
  )
})

test_that("otel_is_tracing_enabled accepts custom tracer", {
  custom_tracer <- create_mock_tracer()
  is_tracing_called <- FALSE

  local_mocked_bindings(
    is_tracing_enabled = function(tracer) {
      is_tracing_called <<- TRUE
      expect_equal(tracer, custom_tracer)
      FALSE
    },
    .package = "otel"
  )

  result <- otel_is_tracing_enabled(custom_tracer)
  expect_true(is_tracing_called)
  expect_false(result)
})

test_that("get_ospan_logger caches logger in non-test environment", {
  mock_logger <- create_mock_logger()
  get_logger_call_count <- 0

  fn_env <- environment(get_ospan_logger)
  # Reset cached logger now and when test ends
  fn_env$reset_logger()
  withr::defer({ fn_env$reset_logger() })

  local_mocked_bindings(
    otel_get_logger = function() {
      get_logger_call_count <<- get_logger_call_count + 1
      mock_logger
    }
  )

  with_mocked_bindings(
    testthat__is_testing = function() TRUE,
    {
      # First call
      logger1 <- get_ospan_logger()
      expect_equal(logger1, mock_logger)
      expect_equal(get_logger_call_count, 1)

      # Second call should call otel::get_logger again (no caching in tests)
      logger2 <- get_ospan_logger()
      expect_equal(logger2, mock_logger)
      expect_equal(get_logger_call_count, 2)  # Incremented
    }
  )

  with_mocked_bindings(
    testthat__is_testing = function() FALSE,
    {
      # First call should call otel::get_logger
      logger1 <- get_ospan_logger()
      expect_equal(logger1, mock_logger)
      expect_equal(get_logger_call_count, 3)

      # Second call should use cached logger
      logger2 <- get_ospan_logger()
      expect_equal(logger2, mock_logger)
      expect_equal(get_logger_call_count, 3)  # Still 3, not incremented
    }
  )
})


test_that("shiny_otel_tracer caches tracer in non-test environment", {
  mock_tracer <- create_mock_tracer()
  get_tracer_call_count <- 0

  fn_env <- environment(shiny_otel_tracer)
  # Reset cached tracer now and when test ends
  fn_env$reset_tracer()
  withr::defer({ fn_env$reset_tracer() })

  local_mocked_bindings(
    otel_get_tracer = function() {
      get_tracer_call_count <<- get_tracer_call_count + 1
      mock_tracer
    }
  )

  with_mocked_bindings(
    testthat__is_testing = function() TRUE,
    {
      # First call
      tracer1 <- shiny_otel_tracer()
      expect_equal(tracer1, mock_tracer)
      expect_equal(get_tracer_call_count, 1)

      # Second call should call otel::get_tracer again (no caching in tests)
      tracer2 <- shiny_otel_tracer()
      expect_equal(tracer2, mock_tracer)
      expect_equal(get_tracer_call_count, 2)  # Incremented
    }
  )

  with_mocked_bindings(
    testthat__is_testing = function() FALSE,
    {
      # First call should call otel::get_tracer
      tracer1 <- shiny_otel_tracer()
      expect_equal(tracer1, mock_tracer)
      expect_equal(get_tracer_call_count, 3)

      # Second call should use cached tracer
      tracer2 <- shiny_otel_tracer()
      expect_equal(tracer2, mock_tracer)
      expect_equal(get_tracer_call_count, 3)  # Still 3, not incremented
    }
  )
})

test_that("integration test - with_shiny_ospan_async uses cached tracer", {
  mock_tracer <- create_mock_tracer()
  get_tracer_call_count <- 0
  with_ospan_async_called <- FALSE

  fn_env <- environment(shiny_otel_tracer)
  # Reset cached tracer now and when test ends
  fn_env$reset_tracer()
  withr::defer({ fn_env$reset_tracer() })

  local_mocked_bindings(
    otel_get_tracer = function() {
      get_tracer_call_count <<- get_tracer_call_count + 1
      mock_tracer
    }
  )

  with_mocked_bindings(
    testthat__is_testing = function() FALSE,
    with_ospan_async = function(name, expr, ..., attributes = NULL, tracer = NULL) {
      with_ospan_async_called <<- TRUE
      expect_equal(tracer, mock_tracer)
      force(expr)
    },
    {
      # First call to with_shiny_ospan_async
      with_shiny_ospan_async("span1", { "result1" })
      expect_equal(get_tracer_call_count, 1)
      expect_true(with_ospan_async_called)

      with_ospan_async_called <- FALSE

      # Second call should use cached tracer
      with_shiny_ospan_async("span2", { "result2" })
      expect_equal(get_tracer_call_count, 1)  # Still 1, tracer was cached
      expect_true(with_ospan_async_called)
    }
  )
})

test_that("integration test - create_shiny_ospan with custom parameters", {
  mock_tracer <- create_mock_tracer()
  mock_span <- create_mock_otel_span()
  start_span_params <- list()

  local_mocked_bindings(
    start_span = function(name, ..., tracer = NULL) {
      start_span_params <<- list(
        name = name,
        tracer = tracer,
        extra_args = list(...)
      )
      mock_span
    },
    .package = "otel"
  )

  with_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
    {
      result <- create_shiny_ospan(
        "custom_span",
        attributes = list(key = "value"),
        parent = "parent_span"
      )

      expect_equal(result, mock_span)
      expect_equal(start_span_params$name, "custom_span")
      expect_equal(start_span_params$tracer, mock_tracer)
      expect_equal(start_span_params$extra_args$attributes, list(key = "value"))
      expect_equal(start_span_params$extra_args$parent, "parent_span")
    }
  )
})
