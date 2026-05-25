# Tests for otel-reactive-update.R functions

# Helper function to create a mock otel span
create_mock_otel_span <- function(name, attributes = NULL, ended = FALSE) {
  structure(
    list(name = name, attributes = attributes, ended = ended),
    class = c("mock_otel_span", "otel_span")
  )
}

test_that("otel_span_reactive_update_init returns early when otel not enabled", {
  domain <- MockShinySession$new()

  # Convince has_otel_collect to return FALSE
  withr::local_options(list(shiny.otel.collect = "none"))

  # Should return early without creating span
  result <- otel_span_reactive_update_init(domain = domain)
  expect_null(result)
  expect_null(domain$userData[["_otel_span_reactive_update"]])
})

test_that("otel_span_reactive_update_init sets up session cleanup on first call", {
  callback_added <- FALSE
  TestMockShinySession <- R6::R6Class(
    "TestMockShinySession",
    inherit = MockShinySession,
    portable = FALSE,
    lock_objects = FALSE,
    public = list(
      # Mock onSessionEnded to track if callback is added
      onSessionEnded = function(callback) {
        callback_added <<- TRUE
        expect_true(is.function(callback))
      }
    )
  )
  domain <- TestMockShinySession$new()

  withr::local_options(list(shiny.otel.collect = "reactive_update"))

  local_mocked_bindings(
    has_otel_collect = function(level) level == "reactive_update",
    start_otel_span = function(name, ..., attributes = NULL) create_mock_otel_span(name, attributes = attributes),
    otel_session_id_attrs = function(domain) list(session_id = "mock-session-id")
  )

  otel_span_reactive_update_init(domain = domain)

  expect_true(callback_added)
  expect_true(domain$userData[["_otel_has_reactive_cleanup"]])
  expect_equal(
    domain$userData[["_otel_span_reactive_update"]],
    create_mock_otel_span("reactive_update", attributes = list(session_id = "mock-session-id"))
  )
})

test_that("otel_span_reactive_update_init errors when span already exists", {
  domain <- MockShinySession$new()
  domain$token <- "mock-session-token"

  # Set up existing span
  existing_otel_span <- create_mock_otel_span("reactive_update", attributes = list(session.id = "mock-session-token"))
  domain$userData[["_otel_span_reactive_update"]] <- existing_otel_span

  local_mocked_bindings(
    has_otel_collect = function(level) level == "reactive_update"
  )

  expect_error(
    otel_span_reactive_update_init(domain = domain),
    "Reactive update span already exists"
  )
})

test_that("otel_span_reactive_update_init doesn't setup cleanup twice", {
  TestMockShinySession <- R6::R6Class(
    "TestMockShinySession",
    inherit = MockShinySession,
    portable = FALSE,
    lock_objects = FALSE,
    public = list(
      # Mock onSessionEnded to track how many times callback is added
      callback_count = 0,
      onSessionEnded = function(callback) {
        self$callback_count <- self$callback_count + 1
        expect_true(is.function(callback))
      }
    )
  )
  domain <- TestMockShinySession$new()

  # Set cleanup flag manually
  domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE

  local_mocked_bindings(
    has_otel_collect = function(level) level == "reactive_update",
    start_otel_span = function(...) create_mock_otel_span("reactive_update")
  )

  otel_span_reactive_update_init(domain = domain)

  # Should not have called onSessionEnded since cleanup was already set
  expect_equal(domain$callback_count, 0)
})

test_that("otel_span_reactive_update_teardown ends span when it exists", {
  domain <- MockShinySession$new()
  mock_otel_span <- create_mock_otel_span("reactive_update")
  domain$userData[["_otel_span_reactive_update"]] <- mock_otel_span

  span_ended <- FALSE

  local_mocked_bindings(
    end_span = function(span) {
      span_ended <<- TRUE
      expect_equal(span, mock_otel_span)
    },
    .package = "otel"
  )

  otel_span_reactive_update_teardown(domain = domain)

  expect_true(span_ended)
  expect_null(domain$userData[["_otel_span_reactive_update"]])
})

test_that("otel_span_reactive_update_teardown handles missing span gracefully", {
  domain <- MockShinySession$new()

  # No span exists
  expect_null(domain$userData[["_otel_span_reactive_update"]])

  # Should not error
  expect_no_error(otel_span_reactive_update_teardown(domain = domain))
})

test_that("with_otel_span_reactive_update executes expr without span", {
  domain <- MockShinySession$new()

  # No span exists
  test_value <- "initial"

  local_mocked_bindings(
    is_otel_span = function(x) FALSE
  )

  result <- with_otel_span_reactive_update({
    test_value <- "modified"
    "result_value"
  }, domain = domain)

  expect_equal(result, "result_value")
  expect_equal(test_value, "modified")
})

test_that("with_otel_span_reactive_update executes expr with active span", {
  domain <- MockShinySession$new()
  mock_otel_span <- create_mock_otel_span("reactive_update")
  domain$userData[["_otel_span_reactive_update"]] <- mock_otel_span

  span_was_active <- FALSE
  test_value <- "initial"

  local_mocked_bindings(
    with_active_span = function(span, expr) {
      span_was_active <<- TRUE
      expect_equal(span, mock_otel_span)
      force(expr)
    },
    .package = "otel"
  )

  result <- with_otel_span_reactive_update({
    test_value <- "modified"
    "result_value"
  }, domain = domain)

  expect_true(span_was_active)
  expect_equal(result, "result_value")
  expect_equal(test_value, "modified")
})

test_that("session cleanup callback works correctly", {
  TestMockShinySession <- R6::R6Class(
    "TestMockShinySession",
    inherit = MockShinySession,
    portable = FALSE,
    lock_objects = FALSE,
    public = list(
      # Mock onSessionEnded to capture the callback
      onSessionEnded = function(callback) {
        self$cleanup_callback <<- callback
      },
      cleanup_callback = NULL
    )
  )
  domain <- TestMockShinySession$new()
  mock_otel_span <- create_mock_otel_span("reactive_update")

  with_mocked_bindings(
    has_otel_collect = function(level) level == "reactive_update",
    start_otel_span = function(...) mock_otel_span,
    otel_session_id_attrs = function(domain) list(session_id = "test"),
    {
      otel_span_reactive_update_init(domain = domain)
    }
  )

  # Verify cleanup callback was registered
  expect_true(is.function(domain$cleanup_callback))

  # Set up span and test cleanup
  domain$userData[["_otel_span_reactive_update"]] <- mock_otel_span
  domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE

  span_ended <- FALSE

  with_mocked_bindings(
    otel_span_reactive_update_teardown = function(domain = NULL) {
      span_ended <<- TRUE
    },
    {
      # Execute the cleanup callback
      domain$cleanup_callback()
      expect_true(span_ended)
    }
  )
})
