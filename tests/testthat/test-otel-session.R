# Tests for otel-session.R functions

# Helper function to create a mock domain with request info
create_mock_session_domain <- function(
  token = "test-session-123",
  request = list(),
  session_ended_callbacks = list()
) {
  TestMockShinySession <- R6::R6Class(
    "TestMockShinySession",
    inherit = MockShinySession,
    portable = FALSE,
    lock_objects = FALSE,
    public = list(
      # Mock onSessionEnded to capture the callback
      onSessionEnded = function(callback) {
        expect_true(is.function(callback))
        self$cleanup_callbacks <- c(self$cleanup_callbacks, list(callback))
      },
      cleanup_callbacks = NULL,
      request_val = NULL
    ),
    active = list(
      request = function(value) {
        if (!missing(value)) {
          self$request_val <- value
        } else {
          self$request_val
        }
      }

    )
  )

  domain <- TestMockShinySession$new()

  domain$request <- request
  domain$token <- token

  domain
}

test_that("with_hybrid_session_start_ospan returns early when otel not enabled", {
  domain <- create_mock_session_domain()
  test_value <- "initial"

  # Mock has_otel_bind to return FALSE
  withr::local_options(list(shiny.otel.bind = "none"))

  result <- with_hybrid_session_start_ospan({
    test_value <- "modified"
    "result_value"
  }, domain = domain)

  expect_equal(result, "result_value")
  expect_equal(test_value, "modified")
  # Should not have registered any callbacks
  expect_length(domain$cleanup_callbacks, 0)
})

test_that("with_hybrid_session_start_ospan sets up session end callback", {
  domain <- create_mock_session_domain(
    token = "session-456",
    request = list(PATH_INFO = "/app", HTTP_HOST = "localhost")
  )

  test_value <- "initial"

  # Mock dependencies
  withr::local_options(list(shiny.otel.bind = "session"))

  local_mocked_bindings(
    as_attributes = function(x) x,
    .package = "otel"
  )

  with_mocked_bindings(
    has_otel_bind = function(level) level == "session",
    otel_session_id_attrs = function(domain) list(session.id = domain$token),
    otel_session_attrs = function(domain) list(PATH_INFO = "/app"),
    with_hybrid_shiny_ospan = function(name, expr, attributes = NULL) {
      expect_equal(name, "session_start")
      expect_true("session.id" %in% names(attributes))
      expect_equal(attributes[["session.id"]], "session-456")
      force(expr)
    },
    {

      expect_length(domain$cleanup_callbacks, 0)

      result <- with_hybrid_session_start_ospan({
        test_value <- "modified"
        "result_value"
      }, domain = domain)

      expect_equal(result, "result_value")
      expect_equal(test_value, "modified")
      expect_length(domain$cleanup_callbacks, 0)

    }
  )
})

test_that("with_hybrid_session_end_ospan returns early when otel not enabled", {
  domain <- create_mock_session_domain()
  test_value <- "initial"

  # Mock has_otel_bind to return FALSE
  withr::local_options(list(shiny.otel.bind = "none"))

  result <- with_hybrid_session_end_ospan({
    test_value <- "modified"
    "result_value"
  }, domain = domain)

  expect_equal(result, "result_value")
  expect_equal(test_value, "modified")
})

test_that("with_hybrid_session_end_ospan creates span when enabled", {
  domain <- create_mock_session_domain(token = "session-end-test")

  span_created <- FALSE
  test_value <- "initial"

  # Mock dependencies
  withr::local_options(list(shiny.otel.bind = "session"))

  with_mocked_bindings(
    has_otel_bind = function(level) level == "session",
    otel_session_id_attrs = function(domain) list(session.id = domain$token),
    with_hybrid_shiny_ospan = function(name, expr, attributes = NULL) {
      span_created <<- TRUE
      expect_equal(name, "session_end")
      expect_equal(attributes[["session.id"]], "session-end-test")
      force(expr)
    },
    {
      result <- with_hybrid_session_end_ospan({
        test_value <- "modified"
        "result_value"
      }, domain = domain)

      expect_equal(result, "result_value")
      expect_equal(test_value, "modified")
      expect_true(span_created)
    }
  )
})

test_that("otel_session_attrs extracts request attributes correctly", {
  # Test with full request info
  domain <- create_mock_session_domain(
    request = list(
      PATH_INFO = "/myapp/page",
      HTTP_HOST = "example.com",
      HTTP_ORIGIN = "https://example.com",
      SERVER_PORT = "8080"
    )
  )

  attrs <- otel_session_attrs(domain)

  expect_equal(attrs$server.path, "/myapp/page")
  expect_equal(attrs$server.address, "example.com")
  expect_equal(attrs$server.origin, "https://example.com")
  expect_equal(attrs$server.port, 8080L)  # Should be converted to integer
})

test_that("otel_session_attrs handles websocket PATH_INFO", {
  domain <- create_mock_session_domain(
    request = list(
      PATH_INFO = "/myapp/websocket/",
      HTTP_HOST = "localhost"
    )
  )

  attrs <- otel_session_attrs(domain)

  # Should strip websocket suffix
  expect_equal(attrs$server.path, "/myapp/")
})

test_that("otel_session_attrs handles missing request fields", {
  # Test with minimal request info
  domain <- create_mock_session_domain(
    request = list(
      HTTP_HOST = "localhost"
    )
  )

  attrs <- otel_session_attrs(domain)

  expect_equal(attrs$server.path, "")
  expect_equal(attrs$server.address, "localhost")
  expect_equal(attrs$server.origin, "")
  expect_equal(attrs$server.port, NA_integer_)
})

test_that("otel_session_attrs handles empty request", {
  domain <- create_mock_session_domain(request = list())

  attrs <- otel_session_attrs(domain)

  expect_equal(attrs$server.path, "")
  expect_equal(attrs$server.address, "")
  expect_equal(attrs$server.origin, "")
  expect_equal(attrs$server.port, NA_integer_)
})

test_that("otel_session_attrs handles invalid SERVER_PORT gracefully", {
  domain <- create_mock_session_domain(
    request = list(SERVER_PORT = "invalid")
  )

  # Should not error even with invalid port
  attrs <- otel_session_attrs(domain)

  # Should remain as string if conversion fails
  expect_equal(attrs$server.port, "invalid")
})

test_that("otel_session_id_attrs returns correct session ID", {
  domain <- create_mock_session_domain(token = "unique-session-token")

  attrs <- otel_session_id_attrs(domain)

  expect_equal(attrs$session.id, "unique-session-token")
  expect_length(attrs, 1)
})

test_that("otel_session_id_attrs handles missing token", {
  domain <- create_mock_session_domain(token = NULL)

  attrs <- otel_session_id_attrs(domain)

  expect_null(attrs$session.id)
})

test_that("integration test - session start with full request", {
  domain <- create_mock_session_domain(
    token = "integration-test-session",
    request = list(
      PATH_INFO = "/dashboard/",
      HTTP_HOST = "shiny.example.com",
      HTTP_ORIGIN = "https://shiny.example.com",
      SERVER_PORT = "3838"
    )
  )

  session_callback <- NULL
  span_attributes <- NULL

  # Mock dependencies
  withr::local_options(list(shiny.otel.bind = "session"))

  local_mocked_bindings(
    as_attributes = function(x) x,
    .package = "otel"
  )

  with_mocked_bindings(
    has_otel_bind = function(level) level == "session",
    otel_session_id_attrs = otel_session_id_attrs,  # Use real function
    otel_session_attrs = otel_session_attrs,        # Use real function
    with_hybrid_shiny_ospan = function(name, expr, attributes = NULL) {
      span_attributes <<- attributes
      force(expr)
    },
    otel_log = function(...) {},  # Mock log function
    {

      expect_length(domain$cleanup_callbacks, 0)

      result <- with_hybrid_session_start_ospan({
        "test_result"
      }, domain = domain)

      expect_equal(result, "test_result")

      # Check span attributes include both session ID and request info
      expect_equal(span_attributes[["session.id"]], "integration-test-session")
      expect_equal(span_attributes[["server.path"]], "/dashboard/")
      expect_equal(span_attributes[["server.address"]], "shiny.example.com")
      expect_equal(span_attributes[["server.port"]], 3838L)
    }
  )
})
