# Tests for otel-shiny.R functions

# Helper function to create a mock otel span
create_mock_otel_span <- function(name = "test_span") {
  structure(
    list(
      name = name,
      activate = function(...) NULL,
      end = function(...) NULL
    ),
    class = "otel_span"
  )
}

# Helper function to create a mock tracer
create_mock_tracer <- function() {
  structure(
    list(
      name = "mock_tracer",
      is_enabled = function() TRUE,
      start_span = function(name, ...) create_mock_otel_span(name)
    ),
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


test_that("start_otel_span calls otel::start_span with correct parameters", {
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
  local_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
  )

  result <- start_otel_span("test_span", extra_param = "value")

  expect_true(start_span_called)
  expect_equal(result, mock_span)
})

test_that("is_otel_span correctly identifies otel spans", {
  # Test with otel_span object
  otel_span <- create_mock_otel_span()
  expect_true(is_otel_span(otel_span))

  # Test with non-otel objects
  expect_false(is_otel_span("string"))
  expect_false(is_otel_span(123))
  expect_false(is_otel_span(list()))
  expect_false(is_otel_span(NULL))

  # Test with object that has different class
  other_obj <- structure(list(), class = "other_class")
  expect_false(is_otel_span(other_obj))
})

test_that("testthat__is_testing detects testing environment", {
  # Test when TESTTHAT env var is set to "true"
  withr::with_envvar(list(TESTTHAT = "true"), {
    expect_true(testthat__is_testing())
  })

  # Test when TESTTHAT env var is not set
  withr::with_envvar(list(TESTTHAT = NA), {
    expect_false(testthat__is_testing())
  })

  # Test when TESTTHAT env var is set to other values
  withr::with_envvar(list(TESTTHAT = "false"), {
    expect_false(testthat__is_testing())
  })

  withr::with_envvar(list(TESTTHAT = ""), {
    expect_false(testthat__is_testing())
  })
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
  local_mocked_bindings(
    shiny_otel_logger = function() mock_logger,
  )

  otel_log("test message", severity = "warn")
  expect_true(log_called)
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
  local_mocked_bindings(
    shiny_otel_logger = function() mock_logger,
  )

  otel_log("default test")
  expect_true(log_called)
})


test_that("integration test - start_otel_span with custom parameters", {
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
  local_mocked_bindings(
    shiny_otel_tracer = function() mock_tracer,
  )

  result <- start_otel_span(
    "custom_span",
    attributes = list(key = "value"),
    parent = "parent_span"
  )

  expect_equal(result, mock_span)
  expect_equal(start_span_params$name, "custom_span")
  expect_equal(start_span_params$tracer, mock_tracer)
  expect_equal(start_span_params$extra_args$attributes, list(key = "value"))
  expect_equal(start_span_params$extra_args$parent, "parent_span")
})
