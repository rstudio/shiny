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
