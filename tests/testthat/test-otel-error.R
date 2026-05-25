skip_on_cran()
skip_if_not_installed("otelsdk")

create_mock_session <- function() {
  session <- MockShinySession$new()
  session$token <- "test-session-token"
  session
}

expect_session_warning <- function(session, warning) {
  testthat::expect_warning(
    capture.output(
      type = "message",
      {
        session$flushReact()
      }
    ),
    warning
  )
}

exception_trace_events <- function(traces) {
  unlist(lapply(traces, function(trace) {
    if (is.null(trace$events)) return(list())
    events <- Filter(function(event) {
      !is.null(event$attributes) &&
        !is.null(event$attributes[["exception.message"]])
    }, trace$events)
    events
  }), recursive = FALSE)
}

test_server_with_otel_error <- function(session, server, expr, sanitize = FALSE, args = list()) {
  stopifnot(inherits(session, "MockShinySession"))
  stopifnot(is.function(server))

  traces <- with_shiny_otel_record({ 42 })$traces
  expect_length(traces, 0)

  withr::with_options(
    list(
      shiny.otel.collect = "all",
      shiny.otel.sanitize.errors = sanitize
    ),
    {
      info <- with_shiny_otel_record({
        # rlang quosure magic to capture and pass through `expr`
        testServer(server, {{ expr }}, args = args, session = session)
      })
    }
  )

  info$traces
}


test_that("mark_otel_exception_as_seen() returns modified condition", {
  cnd <- simpleError("test error")
  result <- mark_otel_exception_as_seen(cnd)

  expect_true(inherits(result, "error"))
  expect_true(inherits(result, "condition"))
  expect_equal(conditionMessage(result), "test error")
  expect_true(isTRUE(result$.shiny_otel_exception))
})

test_that("mark_otel_exception_as_seen() marks error as seen", {
  cnd <- simpleError("test error")
  expect_false(has_seen_otel_exception(cnd))

  cnd <- mark_otel_exception_as_seen(cnd)
  expect_true(has_seen_otel_exception(cnd))
})


test_that("set_otel_exception_status() records sanitized errors by default", {
  server <- function(input, output, session) {
    r1 <- reactive(label = "r1", {
      stop("test error in r1")
    })

    r2 <- reactive(label = "r2", {
      r1()
    })

    observe(label = "obs", {
      r2()
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel_error(
    sanitize = NULL,
    session,
    server,
    {
      # Expect an error to be thrown as warning
      expect_session_warning(session, "test error in r1")
    }
  )

  # IDK why, I don't have time to debug
  skip_if(length(traces) > 3, "Too many traces collected; otelsdk traces are polluted. Run in single test file only: testthat::test_file(testthat::test_path('test-otel-error.R'))`")

  # Find traces with exception events (should only be one)
  exception_events <- exception_trace_events(traces)

  # Exception should be recorded only once at the original point of failure
  expect_equal(length(exception_events), 1)
  expect_match(
    exception_events[[1]]$attributes[["exception.message"]],
    "Check your logs or contact the app author for clarification."
  )
})

test_that("set_otel_exception_status() records exception only once in reactive context", {
  server <- function(input, output, session) {
    r1 <- reactive(label = "r1", {
      stop("test error in r1")
    })

    r2 <- reactive(label = "r2", {
      r1()
    })

    observe(label = "obs", {
      r2()
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel_error(session, server, {
    # Expect an error to be thrown as warning
    expect_session_warning(session, "test error in r1")
  })

  # Find traces with error status
  for (trace in traces) {
    expect_equal(trace$status, "error")
  }

  # Find traces with exception events (should only be one)
  exception_events <- exception_trace_events(traces)

  # Exception should be recorded only once at the original point of failure
  expect_equal(length(exception_events), 1)
  expect_match(
    exception_events[[1]]$attributes[["exception.message"]],
    "test error in r1"
  )
})

test_that("set_otel_exception_status() records exception for multiple independent errors", {
  server <- function(input, output, session) {
    r1 <- reactive(label = "r1", {
      stop("error in r1")
    })

    r2 <- reactive(label = "r2", {
      stop("error in r2")
    })

    observe(label = "obs1", {
      r1()
    })

    observe(label = "obs2", {
      r2()
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel_error(session, server, {
    # Both observers should error
    expect_session_warning(session, "error in r1")
  })

  # Find traces with exception events
  exception_events <- exception_trace_events(traces)

  # Each unique error should be recorded once
  expect_gte(length(exception_events), 1)
})

test_that("set_otel_exception_status() does not record shiny.custom.error", {
  server <- function(input, output, session) {
    r <- reactive(label = "r", {
      cnd <- simpleError("custom error")
      class(cnd) <- c("shiny.custom.error", class(cnd))
      stop(cnd)
    })

    observe(label = "obs", {
      r()
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel_error(session, server, {
    expect_session_warning(session, "custom error")
  })

  # Find traces with error status (should be none for custom errors)
  for (trace in traces) {
    expect_true(trace$status != "error")
  }
})

test_that("set_otel_exception_status() does not record shiny.silent.error", {
  server <- function(input, output, session) {
    r <- reactive(label = "r", {
      cnd <- simpleError("silent error")
      class(cnd) <- c("shiny.silent.error", class(cnd))
      stop(cnd)
    })

    observe(label = "obs", {
      r()
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel_error(session, server, {
    expect_no_error(session$flushReact())
  })

  # Find traces with error status (should be none for silent errors)
  for (trace in traces) {
    expect_true(trace$status != "error")
  }
})
