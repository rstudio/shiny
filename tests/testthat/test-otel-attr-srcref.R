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

  # Debounce and throttle
  r_debounce <- reactive({ 48 }) |> debounce(1000)
  r_throttle <- reactive({ 49 }) |> throttle(1000)

  # ExtendedTask
  ext_task <- ExtendedTask$new(function() { promises::promise_resolve(50) })

  # Reactive with explicit label
  r_labeled <- reactive({ 51 }, label = "my_reactive")
  o_labeled <- observe({ 52 }, label = "my_observer")

  # Poll and File
  r_poll <- reactivePoll(1000, NULL, checkFunc = function() { TRUE}, valueFunc = function() { 53 })
  r_file <- reactiveFileReader(1000, NULL, filePath = "path/to/file")

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
    observeEventB = o2b,
    debounce = r_debounce,
    throttle = r_throttle,
    extendedTask = ext_task,
    reactiveLabeled = r_labeled,
    observeLabeled = o_labeled,
    reactivePoll = r_poll,
    reactiveFileReader = r_file
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

  expect_equal(attrs[["code.file.path"]], "/path/to/myfile.R")
  expect_equal(attrs[["code.line.number"]], 15)
  expect_equal(attrs[["code.column.number"]], 8)
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

      expect_equal(attrs[["code.file.path"]], "function_file.R")
      expect_equal(attrs[["code.line.number"]], 42)
      expect_equal(attrs[["code.column.number"]], 12)
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
  expect_equal(attrs[["code.line.number"]], 10)
  expect_equal(attrs[["code.column.number"]], 5)
  expect_false("code.file.path" %in% names(attrs))
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
  expect_equal(attrs[["code.line.number"]], 10)
  expect_equal(attrs[["code.column.number"]], 5)
  expect_false("code.file.path" %in% names(attrs))
})

# Integration tests with reactive functions
test_that("reactive() captures otel attributes from source reference", {
  # This test verifies that reactive() functions get otel attributes set
  # We'll need to mock the internals since we can't easily control srcref in tests

  x <- get_reactive_objects()$reactive
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 4)
  expect_equal(attrs[["code.column.number"]], 3)
})

test_that("reactiveVal() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactiveVal

  # Test the attribute extraction that would be used in reactiveVal
  attrs <- attr(x, ".impl")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 5)
  expect_equal(attrs[["code.column.number"]], 3)
})

test_that("reactiveValues() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactiveValues

  attrs <- .subset2(x, "impl")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 6)
  expect_equal(attrs[["code.column.number"]], 3)
})

test_that("observe() captures otel attributes from source reference", {
  x <- get_reactive_objects()$observe
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 7)
  expect_equal(attrs[["code.column.number"]], 3)
})

test_that("otel attributes integration with render functions", {
  x <- get_reactive_objects()$renderText
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 8)
  expect_equal(attrs[["code.column.number"]], 20)
})

test_that("observeEvent() captures otel attributes from source reference", {
  x <- get_reactive_objects()$observeEvent
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 9)
  expect_equal(attrs[["code.column.number"]], 3)
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
  expect_true("code.file.path" %in% names(attrs))
  expect_true("code.line.number" %in% names(attrs))
  expect_true("code.column.number" %in% names(attrs))

  # Check that values are of correct types
  expect_true(is.character(attrs[["code.file.path"]]))
  expect_true(is.numeric(attrs[["code.line.number"]]))
  expect_true(is.numeric(attrs[["code.column.number"]]))
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
  expect_false("code.file.path" %in% names(attrs_no_file))
})

test_that("otel attributes are used in reactive context execution", {
  # Test that otel attributes are properly passed through to spans
  mock_attrs <- list(
    "code.file.path" = "context_test.R",
    "code.line.number" = 42L,
    "code.column.number" = 8L
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
    "code.file.path" = "session_test.R",
    "code.line.number" = 15L,
    "code.column.number" = 5L
  )

  session_attrs <- list(
    "session.id" = "test-session-123"
  )

  # Simulate the combination as done in reactives.R
  combined_attrs <- c(srcref_attrs, session_attrs)

  expect_equal(length(combined_attrs), 4)
  expect_equal(combined_attrs[["code.file.path"]], "session_test.R")
  expect_equal(combined_attrs[["code.line.number"]], 15L)
  expect_equal(combined_attrs[["session.id"]], "test-session-123")
})

test_that("eventReactive() captures otel attributes from source reference", {
  x <- get_reactive_objects()$eventReactive
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 10)
  expect_equal(attrs[["code.column.number"]], 3)
})

test_that("renderText() with bindCache() captures otel attributes", {
  x <- get_reactive_objects()$renderCacheA
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("renderText() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$renderEventA
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that(
  "renderText() with bindCache() |> bindEvent() captures otel attributes",
  {
    x <- get_reactive_objects()$renderCacheEventA
    attrs <- attr(x, "otelAttrs")

    expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.line.number"]], 12)
  }
)

test_that("bindCache() wrapping renderText() captures otel attributes", {
  x <- get_reactive_objects()$renderCacheB
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("bindEvent() wrapping renderText() captures otel attributes", {
  x <- get_reactive_objects()$renderEventB
  attrs <- attr(x, "otelAttrs")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that(
  "bindEvent() wrapping bindCache(renderText()) captures otel attributes",
  {
    x <- get_reactive_objects()$renderCacheEventB
    attrs <- attr(x, "otelAttrs")

    expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.line.number"]], 12)
  }
)

test_that("observe() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$observeEventA
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("bindEvent() wrapping observe() captures otel attributes", {
  x <- get_reactive_objects()$observeEventB
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("reactive() with bindCache() captures otel attributes", {
  x <- get_reactive_objects()$reactiveCacheA
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("reactive() with bindEvent() captures otel attributes", {
  x <- get_reactive_objects()$reactiveEventA
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that(
  "reactive() with bindCache() |> bindEvent() captures otel attributes",
  {
    x <- get_reactive_objects()$reactiveCacheEventA
    attrs <- attr(x, "observable")$.otelAttrs

    expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.line.number"]], 12)
  }
)

test_that("bindCache() wrapping reactive() captures otel attributes", {
  x <- get_reactive_objects()$reactiveCacheB
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("bindEvent() wrapping reactive() captures otel attributes", {
  x <- get_reactive_objects()$reactiveEventB
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that(
  "bindEvent() wrapping bindCache(reactive()) captures otel attributes",
  {
    x <- get_reactive_objects()$reactiveCacheEventB
    attrs <- attr(x, "observable")$.otelAttrs

    expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
    expect_gt(attrs[["code.line.number"]], 12)
  }
)

# Tests for debounce/throttle
test_that("debounce() creates new reactive with otel attributes", {
  x <- get_reactive_objects()$debounce
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

test_that("throttle() creates new reactive with otel attributes", {
  x <- get_reactive_objects()$throttle
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

# Tests for ExtendedTask
test_that("ExtendedTask is created and is an R6 object", {
  x <- get_reactive_objects()$extendedTask
  expect_s3_class(x, "ExtendedTask")
  expect_s3_class(x, "R6")

  attrs <- .subset2(x, ".__enclos_env__")$private$otel_attrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

# Tests for reactivePoll
test_that("reactivePoll() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactivePoll
  impl <- attr(x, "observable", exact = TRUE)
  attrs <- impl$.otelAttrs
  otelLabel <- impl$.otelLabel

  expect_equal(as.character(otelLabel), "reactivePoll r_poll")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

# Tests for reactiveFileReader
test_that("reactiveFileReader() captures otel attributes from source reference", {
  x <- get_reactive_objects()$reactiveFileReader
  impl <- attr(x, "observable", exact = TRUE)
  attrs <- impl$.otelAttrs
  otelLabel <- impl$.otelLabel

  expect_equal(as.character(otelLabel), "reactiveFileReader r_file")

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_gt(attrs[["code.line.number"]], 12)
})

# Tests for explicit labels
test_that("reactive() with explicit label still captures otel attributes", {
  x <- get_reactive_objects()$reactiveLabeled
  attrs <- attr(x, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 38)
  expect_equal(attrs[["code.column.number"]], 3)

  # Verify label is preserved
  label <- attr(x, "observable")$.label
  expect_equal(as.character(label), "my_reactive")
})

test_that("observe() with explicit label still captures otel attributes", {
  x <- get_reactive_objects()$observeLabeled
  attrs <- x$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_equal(attrs[["code.line.number"]], 39)
  expect_equal(attrs[["code.column.number"]], 3)

  # Verify label is preserved
  expect_equal(x$.label, "my_observer")
})

# Edge case tests
test_that("reactive created inside function captures function srcref", {
  create_reactive <- function() {
    reactive({ 100 })
  }

  r <- create_reactive()
  attrs <- attr(r, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  # Line number should point to where reactive() is called inside the function
  expect_true(is.numeric(attrs[["code.line.number"]]))
  expect_true(is.numeric(attrs[["code.column.number"]]))
})

test_that("observe created inside function captures function srcref", {
  create_observer <- function() {
    observe({ 101 })
  }

  o <- create_observer()
  attrs <- o$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_true(is.numeric(attrs[["code.line.number"]]))
  expect_true(is.numeric(attrs[["code.column.number"]]))
})

test_that("reactive returned from function preserves srcref", {
  make_counter <- function(initial = 0) {
    reactive({ initial + 1 })
  }

  counter <- make_counter(42)
  attrs <- attr(counter, "observable")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_true(is.numeric(attrs[["code.line.number"]]))
})

test_that("reactiveVal created in function captures srcref", {
  create_val <- function() {
    reactiveVal("initial")
  }

  rv <- create_val()
  attrs <- attr(rv, ".impl")$.otelAttrs

  expect_equal(attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_true(is.numeric(attrs[["code.line.number"]]))
})

test_that("nested reactive expressions preserve individual srcrefs", {
  outer_reactive <- reactive({
    inner_reactive <- reactive({ 200 })
    inner_reactive
  })

  outer_attrs <- attr(outer_reactive, "observable")$.otelAttrs
  expect_equal(outer_attrs[["code.file.path"]], "test-otel-attr-srcref.R")
  expect_true(is.numeric(outer_attrs[["code.line.number"]]))

  # Get the inner reactive by executing outer
  withReactiveDomain(MockShinySession$new(), {
    inner_reactive <- isolate(outer_reactive())
    inner_attrs <- attr(inner_reactive, "observable")$.otelAttrs

    expect_equal(inner_attrs[["code.file.path"]], "test-otel-attr-srcref.R")
    expect_true(is.numeric(inner_attrs[["code.line.number"]]))
    # Inner should have different line number than outer
    expect_false(inner_attrs[["code.line.number"]] == outer_attrs[["code.line.number"]])
  })
})
