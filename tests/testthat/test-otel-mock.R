skip_on_cran()
skip_if_not_installed("otelsdk")

expect_code_attrs <- function(trace, expected_fn_name = NULL) {
  testthat::expect_true(!is.null(trace))
  testthat::expect_true(is.list(trace$attributes))
  testthat::expect_true(is.character(trace$attributes[["code.file.path"]]))
  testthat::expect_equal(trace$attributes[["code.file.path"]], "test-otel-mock.R")
  testthat::expect_true(is.numeric(trace$attributes[["code.line.number"]]))
  testthat::expect_true(is.numeric(trace$attributes[["code.column.number"]]))

  # Check code.function.name if expected
  if (!is.null(expected_fn_name)) {
    testthat::expect_true(
      is.character(trace$attributes[["code.function.name"]])
    )
    testthat::expect_equal(
      trace$attributes[["code.function.name"]],
      expected_fn_name
    )
  }

  invisible(trace)
}
MOCK_SESSION_TOKEN <- "test-session-token"
expect_session_id <- function(trace) {
  testthat::expect_true(!is.null(trace))
  testthat::expect_true(is.list(trace$attributes))
  testthat::expect_true(is.character(trace$attributes[["session.id"]]))
  testthat::expect_equal(trace$attributes[["session.id"]], MOCK_SESSION_TOKEN)

  invisible(trace)
}

expect_trace <- function(traces, name, pos = 1, expected_fn_name = NULL) {
  # Filter to traces with the given name
  trace_set <- traces[which(names(traces) == name)]
  testthat::expect_gte(length(trace_set), pos)

  # Get the trace at the given position
  trace <- trace_set[[pos]]
  testthat::expect_true(is.list(trace))

  expect_code_attrs(trace, expected_fn_name = expected_fn_name)
  expect_session_id(trace)

  trace
}

create_mock_session <- function() {
  session <- MockShinySession$new()
  session$token <- MOCK_SESSION_TOKEN
  session
}

test_server_with_otel <- function(session, server, expr, bind = "all", args = list()) {
  stopifnot(inherits(session, "MockShinySession"))
  stopifnot(is.function(server))

  withr::with_options(list(shiny.otel.collect = bind), {
    info <- with_shiny_otel_record({
      # rlang quosure magic to capture and pass through `expr`
      testServer(server, {{ expr }}, args = args, session = session)
    })
  })

  info$traces
}

for (bind in c("all", "reactivity")) {
  test_that(paste0("bind='", bind, "' handles observers"), {
    server <- function(input, output, session) {
      observe({
        42
      })

      my_observe <- observe({
        43
      })

      observe({
        44
      }, label = "labeled observer")
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      # probably not needed to do anything here
      session$flushReact()
    })

    expect_trace(traces, "observe mock-session:<anonymous>", 1, "observe")
    expect_trace(traces, "observe mock-session:my_observe", 1, "observe")
    expect_trace(traces, "observe mock-session:labeled observer", 1, "observe")
  })

  test_that(paste0("bind='", bind, "' handles reactiveVal / reactiveValues"), {
    server <- function(input, output, session) {
      rv <- reactiveVal(0)
      rv2 <- (function() {reactiveVal(0)})()  # test anonymous reactiveVal
      rv3 <- reactiveVal(0, "labeled_rv")

      observe({
        isolate({
          rv(rv() + 1)
          rv2(rv2() + 1)
          rv3(rv3() + 1)
        })
      })
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      session$flushReact()
      expect_equal(rv(), 1)
    })

    expect_trace(traces, "observe mock-session:<anonymous>", 1, "observe")

    # TODO-future: Add tests to see the `Set reactiveVal mock-session:rv` logs
    # Requires: https://github.com/r-lib/otelsdk/issues/21
  })

  test_that(paste0("bind='", bind, "' handles reactive"), {
    server <- function(input, output, session) {
      r <- reactive({ 42 })
      r2 <- (function() {reactive({ r() })})()  # test anonymous reactive
      r3 <- reactive({ r2() }, label = "labeled_rv")

      observe(label = "obs_r3", {
        r3()
      })
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      session$flushReact()
      session$flushReact()
      session$flushReact()
      expect_equal(r(), 42)
      expect_equal(r2(), 42)
      expect_equal(r3(), 42)
    })

    observe_trace <- expect_trace(
      traces, "observe mock-session:obs_r3", 1, "observe"
    )
    r_trace <- expect_trace(traces, "reactive mock-session:r", 1, "reactive")
    r2_trace <- expect_trace(
      traces, "reactive mock-session:<anonymous>", 1, "reactive"
    )
    r3_trace <- expect_trace(
      traces, "reactive mock-session:labeled_rv", 1, "reactive"
    )

    expect_equal(r_trace$parent, r2_trace$span_id)
    expect_equal(r2_trace$parent, r3_trace$span_id)
    expect_equal(r3_trace$parent, observe_trace$span_id)
  })


  test_that(paste0("bind='", bind, "' outputs are supported"), {
    server <- function(input, output, session) {
      output$txt <- renderText({
        "Hello, world!"
      })
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      session$flushReact()
      session$flushReact()
      session$flushReact()
      expect_equal(output$txt, "Hello, world!")
    })

    # Outputs (render functions) should NOT have code.function.name
    trace <- expect_trace(traces, "output mock-session:txt", 1, NULL)
    expect_false("code.function.name" %in% names(trace$attributes))
  })

  test_that(paste0("bind='", bind, "' extended tasks are supported"), {
    server <- function(input, output, session) {
      rand_task <- ExtendedTask$new(function() {
        promise_resolve(42) |> promises::then(function(value) {
          value
        })
      })

      observe(label = "invoke task", {
        rand_task$invoke()
      })

      output$result <- renderText({
        # React to updated results when the task completes
        number <- rand_task$result()
        paste0("Your number is ", number, ".")
      })
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      session$flushReact()

      while (!later::loop_empty()) {
        later::run_now()
        session$flushReact()
      }
      session$flushReact()
    })

    invoke_obs <- expect_trace(
      traces, "observe mock-session:invoke task", 1, "observe"
    )
    # Render functions should NOT have code.function.name
    render1_trace <- expect_trace(traces, "output mock-session:result", 1, NULL)
    expect_false("code.function.name" %in% names(render1_trace$attributes))

    ex_task_trace <- expect_trace(
      traces, "ExtendedTask mock-session:rand_task", 1, "ExtendedTask"
    )

    render2_trace <- expect_trace(
      traces, "output mock-session:result", pos = 2, NULL
    )
    expect_false("code.function.name" %in% names(render2_trace$attributes))

    expect_equal(invoke_obs$span_id, ex_task_trace$parent)
  })

}


test_that("bind = 'reactivity' traces reactive components", {
  server <- function(input, output, session) {
    r <- reactive({ 42 })

    observe(label = "test_obs", {
      r()
    })

    output$txt <- renderText({
      "Hello"
    })
  }

  session <- create_mock_session()
  traces <- test_server_with_otel(session, server, bind = "reactivity", {
    session$flushReact()
    expect_equal(r(), 42)
  })

  # Should trace reactive components (equivalent to "all")
  expect_trace(traces, "observe mock-session:test_obs", 1, "observe")
  expect_trace(traces, "reactive mock-session:r", 1, "reactive")
  # Render functions should NOT have code.function.name
  txt_trace <- expect_trace(traces, "output mock-session:txt", 1, NULL)
  expect_false("code.function.name" %in% names(txt_trace$attributes))
})


for (bind in c("reactive_update", "session", "none")) {
  test_that(paste0("bind = '", bind, "' traces reactive components"), {
    server <- function(input, output, session) {
      r <- reactive({ 42 })

      observe(label = "test_obs", {
        r()
      })

      output$txt <- renderText({
        "Hello"
      })
    }

    session <- create_mock_session()
    traces <- test_server_with_otel(session, server, bind = bind, {
      session$flushReact()
      expect_equal(r(), 42)
    })
    trace_names <- names(traces)

    expect_false(any(grepl("observe", trace_names)))
    expect_false(any(grepl("reactive", trace_names)))
    expect_false(any(grepl("output", trace_names)))
  })
}
