# Do not move or rearrange this code - it defines helper functions used in multiple tests below
get_reactive_objects <- function() {
  # Must use variables, otherwise the source reference is collapsed to a single line
  r <- reactive({ 42 })
  rv <- reactiveVal("test")
  rvs <- reactiveValues(a = 1)
  o <- observe({ 43 })
  rt <- renderText({ "text" })
  oe <- observeEvent({"key"}, { 45 })
  er <- eventReactive({"key"}, { 46 })

  # Values below this line are to test file location, not file line
  r1a <- reactive({ 1 }) |> bindCache({"key"})
  r2a <- reactive({ 2 }) |> bindEvent({"key"})
  r3a <- reactive({ 3 }) |> bindCache({"key1"}) |> bindEvent({"key2"})
  r1b <- bindCache(reactive({ 1 }), {"key"})
  r2b <- bindEvent(reactive({ 2 }), {"key"})
  r3b <- bindEvent(bindCache(reactive({ 3 }), {"key1"}), {"key2"})

  rt1a <- renderText({"text"}) |> bindCache({"key"})
  rt2a <- renderText({"text"}) |> bindEvent({"key"})
  rt3a <- renderText({"text"}) |> bindCache({"key1"}) |> bindEvent({"key2"})
  rt1b <- bindCache(renderText({"text"}), {"key"})
  rt2b <- bindEvent(renderText({"text"}), {"key"})
  rt3b <- bindEvent(bindCache(renderText({"text"}), {"key1"}), {"key2"})

  o2a <- observe({ 44 }) |> bindEvent({"key"})
  o2b <- bindEvent(observe({ 47 }), {"key"})

  list(
    reactive = r,
    reactiveVal = rv,
    reactiveValues = rvs,
    observe = o,
    renderText = rt,
    observeEvent = oe,
    eventReactive = er,
    reactiveCacheA = r1a,
    reactiveEventA = r2a,
    reactiveCacheEventA = r3a,
    reactiveCacheB = r1b,
    reactiveEventB = r2b,
    reactiveCacheEventB = r3b,
    renderCacheA = rt1a,
    renderEventA = rt2a,
    renderCacheEventA = rt3a,
    renderCacheB = rt1b,
    renderEventB = rt2b,
    renderCacheEventB = rt3b,
    observeEventA = o2a,
    observeEventB = o2b
  )
}



# Helper function to create a mock srcref
create_mock_srcref <- function(
  lines = c(10, 15),
  columns = c(5, 20),
  filename = "test_file.R"
) {
  srcfile <- list(filename = filename)
  srcref <- structure(
    c(lines[1], columns[1], lines[2], columns[2], columns[1], columns[2]),
    class = "srcref"
  )
  attr(srcref, "srcfile") <- srcfile
  srcref
}


test_that("otel_srcref_attributes extracts attributes from srcref object", {
  srcref <- create_mock_srcref(
    lines = c(15, 18),
    columns = c(8, 25),
    filename = "/path/to/myfile.R"
  )

  attrs <- otel_srcref_attributes(srcref)

  expect_equal(attrs[["code.filepath"]], "/path/to/myfile.R")
  expect_equal(attrs[["code.lineno"]], 15)
  expect_equal(attrs[["code.column"]], 8)
})

test_that("otel_srcref_attributes handles NULL srcref", {
  attrs <- otel_srcref_attributes(NULL)
  expect_null(attrs)
})

test_that("otel_srcref_attributes extracts from function with srcref", {
  mock_func <- function() { "test" }
  srcref <- create_mock_srcref(
    lines = c(42, 45),
    columns = c(12, 30),
    filename = "function_file.R"
  )

  with_mocked_bindings(
    getSrcRefs = function(func) {
      expect_identical(func, mock_func)
      list(list(srcref))
    },
    {
      attrs <- otel_srcref_attributes(mock_func)

      expect_equal(attrs[["code.filepath"]], "function_file.R")
      expect_equal(attrs[["code.lineno"]], 42)
      expect_equal(attrs[["code.column"]], 12)
    }
  )
})

test_that("otel_srcref_attributes handles function without srcref", {
  mock_func <- function() { "test" }

  with_mocked_bindings(
    getSrcRefs = function(func) {
      list(list(NULL))
    },
    {
      attrs <- otel_srcref_attributes(mock_func)
      expect_null(attrs)
    }
  )
})

test_that("otel_srcref_attributes handles function with empty getSrcRefs", {
  mock_func <- function() { "test" }

  with_mocked_bindings(
    getSrcRefs = function(func) {
      list()  # Empty list
    },
    {
      expect_error(
        otel_srcref_attributes(mock_func),
        "subscript out of bounds|attempt to select less than one element"
      )
    }
  )
})

test_that("otel_srcref_attributes validates srcref class", {
  invalid_srcref <- structure(
    c(10, 5, 15, 20, 5, 20),
    class = "not_srcref"
  )

  expect_error(
    otel_srcref_attributes(invalid_srcref),
    "inherits\\(srcref, \"srcref\"\\) is not TRUE"
  )
})

test_that("otel_srcref_attributes drops NULL values", {
  # Create srcref with missing filename
  srcref <- structure(
    c(10, 5, 15, 20, 5, 20),
    class = "srcref"
  )
  attr(srcref, "srcfile") <- list(filename = NULL)

  attrs <- otel_srcref_attributes(srcref)

  # Should only contain lineno and column, not filepath
  expect_equal(length(attrs), 2)
  expect_equal(attrs[["code.lineno"]], 10)
  expect_equal(attrs[["code.column"]], 5)
  expect_false("code.filepath" %in% names(attrs))
})

test_that("otel_srcref_attributes handles missing srcfile", {
  srcref <- structure(
    c(10, 5, 15, 20, 5, 20),
    class = "srcref"
  )
  # No srcfile attribute

  attrs <- otel_srcref_attributes(srcref)

  # Should only contain lineno and column
  expect_equal(length(attrs), 2)
  expect_equal(attrs[["code.lineno"]], 10)
  expect_equal(attrs[["code.column"]], 5)
  expect_false("code.filepath" %in% names(attrs))
})

# Integration tests with reactive functions
test_that("reactive() captures otel attributes from source reference", {
  # This test verifies that reactive() functions get otel attributes set
  # We'll need to mock the internals since we can't easily control srcref in tests

  x <- get_reactive_objects()$reactive
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 4)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("reactiveVal() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactiveVal

  # Test the attribute extraction that would be used in reactiveVal
  attrs <- attr(x, ".impl")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 5)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("reactiveValues() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactiveValues

  attrs <- .subset2(x, "impl")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 6)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("observe() captures otel attributes from source reference", {
  x <- get_reactive_objects()$observe
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 7)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("otel attributes integration with render functions", {
  x <- get_reactive_objects()$renderText
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 8)
  expect_equal(attrs[["code.column"]], 20)
})

test_that("observeEvent() captures otel attributes from source reference", {
  x <- get_reactive_objects()$observeEvent
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 9)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("otel attributes follow OpenTelemetry semantic conventions", {
  # Test that the attribute names follow the official OpenTelemetry conventions
  # https://opentelemetry.io/docs/specs/semconv/registry/attributes/code/

  srcref <- create_mock_srcref(
    lines = c(1, 1),
    columns = c(1, 10),
    filename = "convention_test.R"
  )

  attrs <- otel_srcref_attributes(srcref)

  # Check that attribute names follow the convention
  expect_true("code.filepath" %in% names(attrs))
  expect_true("code.lineno" %in% names(attrs))
  expect_true("code.column" %in% names(attrs))

  # Check that values are of correct types
  expect_true(is.character(attrs[["code.filepath"]]))
  expect_true(is.numeric(attrs[["code.lineno"]]))
  expect_true(is.numeric(attrs[["code.column"]]))
})

test_that("dropNulls helper works correctly in otel_srcref_attributes", {
  # Test with all values present
  srcref <- create_mock_srcref(
    lines = c(5, 8),
    columns = c(3, 15),
    filename = "complete_test.R"
  )

  attrs <- otel_srcref_attributes(srcref)
  expect_equal(length(attrs), 3)

  # Test with missing filename (NULL)
  srcref_no_file <- structure(
    c(5, 3, 8, 15, 3, 15),
    class = "srcref"
  )
  attr(srcref_no_file, "srcfile") <- list(filename = NULL)

  attrs_no_file <- otel_srcref_attributes(srcref_no_file)
  expect_equal(length(attrs_no_file), 2)
  expect_false("code.filepath" %in% names(attrs_no_file))
})

test_that("otel attributes are used in reactive context execution", {
  # Test that otel attributes are properly passed through to spans
  mock_attrs <- list(
    "code.filepath" = "context_test.R",
    "code.lineno" = 42L,
    "code.column" = 8L
  )

  # Test the context info structure used in react.R
  otel_info <- ctx_otel_info_obj(
    isRecordingOtel = TRUE,
    otelLabel = "test_reactive",
    otelAttrs = mock_attrs
  )

  expect_true(otel_info$isRecordingOtel)
  expect_equal(otel_info$otelLabel, "test_reactive")
  expect_equal(otel_info$otelAttrs, mock_attrs)
  expect_equal(class(otel_info), "ctx_otel_info")
})

test_that("otel attributes are combined with session attributes", {
  # Test that otel srcref attributes are properly combined with session attributes
  # as happens in the reactive system

  srcref_attrs <- list(
    "code.filepath" = "session_test.R",
    "code.lineno" = 15L,
    "code.column" = 5L
  )

  session_attrs <- list(
    "session.id" = "test-session-123"
  )

  # Simulate the combination as done in reactives.R
  combined_attrs <- c(srcref_attrs, session_attrs)

  expect_equal(length(combined_attrs), 4)
  expect_equal(combined_attrs[["code.filepath"]], "session_test.R")
  expect_equal(combined_attrs[["code.lineno"]], 15L)
  expect_equal(combined_attrs[["session.id"]], "test-session-123")
})

test_that("eventReactive() captures otel attributes from source reference", {
  x <- get_reactive_objects()$eventReactive
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.lineno"]], 10)
  expect_equal(attrs[["code.column"]], 3)
})

test_that("renderText() with bindCache() captures otel attributes", {
  x <- get_reactive_objects()$renderCacheA
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("renderText() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$renderEventA
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that(
  "renderText() with bindCache() |> bindEvent() captures otel attributes",
  {
    x <- get_reactive_objects()$renderCacheEventA
    attrs <- attr(x, "otelAttrs")

    expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.lineno"]], 12)
  }
)

test_that("bindCache() wrapping renderText() captures otel attributes", {
  x <- get_reactive_objects()$renderCacheB
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("bindEvent() wrapping renderText() captures otel attributes", {
  x <- get_reactive_objects()$renderEventB
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that(
  "bindEvent() wrapping bindCache(renderText()) captures otel attributes",
  {
    x <- get_reactive_objects()$renderCacheEventB
    attrs <- attr(x, "otelAttrs")

    expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.lineno"]], 12)
  }
)

test_that("observe() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$observeEventA
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("bindEvent() wrapping observe() captures otel attributes", {
  x <- get_reactive_objects()$observeEventB
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("reactive() with bindCache() captures otel attributes", {
  x <- get_reactive_objects()$reactiveCacheA
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("reactive() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$reactiveEventA
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that(
  "reactive() with bindCache() |> bindEvent() captures otel attributes",
  {
    x <- get_reactive_objects()$reactiveCacheEventA
    attrs <- attr(x, "observable")$.otelAttrs

    expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.lineno"]], 12)
  }
)

test_that("bindCache() wrapping reactive() captures otel attributes", {
  x <- get_reactive_objects()$reactiveCacheB
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that("bindEvent() wrapping reactive() captures otel attributes", {
  x <- get_reactive_objects()$reactiveEventB
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.lineno"]], 12)
})

test_that(
  "bindEvent() wrapping bindCache(reactive()) captures otel attributes",
  {
    x <- get_reactive_objects()$reactiveCacheEventB
    attrs <- attr(x, "observable")$.otelAttrs

    expect_equal(attrs[["code.filepath"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.lineno"]], 12)
  }
)
