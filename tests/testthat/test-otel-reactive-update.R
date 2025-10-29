# Tests for otel-reactive-update.R functions

# Helper function to create a mock ospan
create_mock_ospan <- function(name, attributes = NULL, ended = FALSE) {
  structure(
    list(name = name, attributes = attributes, ended = ended),
    class = "mock_ospan"
  )
}

# Mock is_ospan function
is_ospan <- function(x) {
  inherits(x, "mock_ospan") && !isTRUE(x$ended)
}


test_that("start_and_store_reactive_update_ospan returns early when otel not enabled", {
  domain <- MockShinySession$new()

  # Mock has_otel_bind to return FALSE
  withr::local_options(list(shiny.otel.bind = "none"))

  # Should return early without creating span
  result <- start_and_store_reactive_update_ospan(domain = domain)
  expect_null(result)
  expect_null(domain$userData[["_otel_reactive_update_ospan"]])
})

test_that("start_and_store_reactive_update_ospan sets up session cleanup on first call", {
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


  # Mock dependencies
  withr::local_options(list(shiny.otel.bind = "reactive_update"))

  with_mocked_bindings(
    has_otel_bind = function(level) level == "reactive_update",
    start_shiny_ospan = function(name, ..., attributes = NULL) create_mock_ospan(name, attributes = attributes),
    otel_session_id_attrs = function(domain) list(session_id = "mock-session-id"),
    {
      start_and_store_reactive_update_ospan(domain = domain)

      expect_true(callback_added)
      expect_true(domain$userData[["_otel_has_reactive_cleanup"]])
      expect_equal(domain$userData[["_otel_reactive_update_ospan"]], create_mock_ospan("reactive_update", attributes = list(session_id = "mock-session-id")))
    }
  )
})

test_that("start_and_store_reactive_update_ospan errors when span already exists", {
  domain <- MockShinySession$new()
  domain$token <- "mock-session-token"

  # Set up existing span
  existing_ospan <- create_mock_ospan("reactive_update", attributes = list(session.id = "mock-session-token"))
  domain$userData[["_otel_reactive_update_ospan"]] <- existing_ospan

  # Mock dependencies
  with_mocked_bindings(
    has_otel_bind = function(level) level == "reactive_update",
    is_ospan = function(x) inherits(x, "mock_ospan"),
    {
      expect_error(
        start_and_store_reactive_update_ospan(domain = domain),
        "Reactive update span already exists"
      )
    }
  )
})

test_that("start_and_store_reactive_update_ospan doesn't setup cleanup twice", {
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

  # Mock dependencies
  mock_ospan <- create_mock_ospan("reactive_update")

  with_mocked_bindings(
    has_otel_bind = function(level) level == "reactive_update",
    start_shiny_ospan = function(...) mock_ospan,
    {
      start_and_store_reactive_update_ospan(domain = domain)

      # Should not have called onSessionEnded since cleanup was already set
      expect_equal(domain$callback_count, 0)
    }
  )
})

test_that("end_and_clear_reactive_update_ospan ends span when it exists", {
  domain <- MockShinySession$new()
  mock_ospan <- create_mock_ospan("reactive_update")
  domain$userData[["_otel_reactive_update_ospan"]] <- mock_ospan

  span_ended <- FALSE

  with_mocked_bindings(
    end_span = function(span) {
      span_ended <<- TRUE
      expect_equal(span, mock_ospan)
    },
    .package = "otel",
    {
      with_mocked_bindings(
        is_ospan = function(x) inherits(x, "mock_ospan") && !isTRUE(x$ended),
        {
          end_and_clear_reactive_update_ospan(domain = domain)

          expect_true(span_ended)
          expect_null(domain$userData[["_otel_reactive_update_ospan"]])
        }
      )
    }
  )
})

test_that("end_and_clear_reactive_update_ospan handles missing span gracefully", {
  domain <- MockShinySession$new()

  # No span exists
  expect_null(domain$userData[["_otel_reactive_update_ospan"]])

  with_mocked_bindings(
    is_ospan = function(x) FALSE,
    {
      # Should not error
      expect_no_error(end_and_clear_reactive_update_ospan(domain = domain))
    }
  )
})

test_that("with_existing_reactive_update_ospan executes expr without span", {
  domain <- MockShinySession$new()

  # No span exists
  test_value <- "initial"

  with_mocked_bindings(
    is_ospan = function(x) FALSE,
    {
      result <- with_existing_reactive_update_ospan({
        test_value <- "modified"
        "result_value"
      }, domain = domain)

      expect_equal(result, "result_value")
      expect_equal(test_value, "modified")
    }
  )
})

test_that("with_existing_reactive_update_ospan executes expr with active span", {
  domain <- MockShinySession$new()
  mock_ospan <- create_mock_ospan("reactive_update")
  domain$userData[["_otel_reactive_update_ospan"]] <- mock_ospan

  span_was_active <- FALSE
  test_value <- "initial"

  local_mocked_bindings(
    with_active_span = function(span, expr) {
      span_was_active <<- TRUE
      expect_equal(span, mock_ospan)
      force(expr)
    },
    .package = "otel"
  )
  local_mocked_bindings(
    is_ospan = function(x) inherits(x, "mock_ospan") && !isTRUE(x$ended)
  )

  result <- with_existing_reactive_update_ospan({
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

  # Mock dependencies and create span with cleanup
  mock_ospan <- create_mock_ospan("reactive_update")

  with_mocked_bindings(
    has_otel_bind = function(level) level == "reactive_update",
    start_shiny_ospan = function(...) mock_ospan,
    otel_session_id_attrs = function(domain) list(session_id = "test"),
    {
      start_and_store_reactive_update_ospan(domain = domain)
    }
  )

  # Verify cleanup callback was registered
  expect_true(is.function(domain$cleanup_callback))

  # Set up span and test cleanup
  domain$userData[["_otel_reactive_update_ospan"]] <- mock_ospan
  domain$userData[["_otel_has_reactive_cleanup"]] <- TRUE

  span_ended <- FALSE

  with_mocked_bindings(
    end_and_clear_reactive_update_ospan = function(domain = NULL) {
      span_ended <<- TRUE
    },
    {
      # Execute the cleanup callback
      domain$cleanup_callback()
      expect_true(span_ended)
    }
  )
})
