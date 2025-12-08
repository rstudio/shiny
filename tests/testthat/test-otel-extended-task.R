# Tests for ExtendedTask otel behavior

ex_task_42 <- function() {
  ExtendedTask$new(function() {
    promises::promise_resolve(42)
  })
}

test_that("ExtendedTask captures otel collection state at initialization", {
  # Test that has_otel_collect is called at init, not at invoke time
  withr::local_options(list(shiny.otel.collect = "reactivity"))

  # Enable otel tracing
  local_mocked_bindings(
    otel_is_tracing_enabled = function() TRUE
  )

  task <- ex_task_42()

  # Check that is_recording_otel is captured at init time
  expect_true(task$.__enclos_env__$private$is_recording_otel)
})

test_that("ExtendedTask sets is_recording_otel to FALSE when otel disabled", {
  # Enable otel tracing
  local_mocked_bindings(
    otel_is_tracing_enabled = function() FALSE
  )

  # Test with all level
  withr::with_options(list(shiny.otel.collect = "all"), {
    task1 <- ex_task_42()
    expect_false(task1$.__enclos_env__$private$is_recording_otel)
  })

  # Test with reactivity level
  withr::with_options(list(shiny.otel.collect = "reactivity"), {
    task1 <- ex_task_42()
    expect_false(task1$.__enclos_env__$private$is_recording_otel)
  })

  # Test with session level (should be FALSE)
  withr::with_options(list(shiny.otel.collect = "session"), {
    task2 <- ex_task_42()
    expect_false(task2$.__enclos_env__$private$is_recording_otel)
  })

  # Test with none level (should be FALSE)
  withr::with_options(list(shiny.otel.collect = "none"), {
    task3 <- ex_task_42()
    expect_false(task3$.__enclos_env__$private$is_recording_otel)
  })
})

test_that("ExtendedTask sets is_recording_otel based on has_otel_collect at init", {
  # Enable otel tracing
  local_mocked_bindings(
    otel_is_tracing_enabled = function() TRUE
  )

  # Test with all level
  withr::with_options(list(shiny.otel.collect = "all"), {
    task1 <- ex_task_42()
    expect_true(task1$.__enclos_env__$private$is_recording_otel)
  })

  # Test with reactivity level
  withr::with_options(list(shiny.otel.collect = "reactivity"), {
    task1 <- ex_task_42()
    expect_true(task1$.__enclos_env__$private$is_recording_otel)
  })

  # Test with session level (should be FALSE)
  withr::with_options(list(shiny.otel.collect = "session"), {
    task2 <- ex_task_42()
    expect_false(task2$.__enclos_env__$private$is_recording_otel)
  })

  # Test with none level (should be FALSE)
  withr::with_options(list(shiny.otel.collect = "none"), {
    task3 <- ex_task_42()
    expect_false(task3$.__enclos_env__$private$is_recording_otel)
  })
})

test_that("ExtendedTask uses init-time otel setting even if option changes later", {

  # Enable otel tracing
  local_mocked_bindings(
    otel_is_tracing_enabled = function() TRUE
  )

  # Test that changing the option after init doesn't affect the task
  withr::with_options(list(shiny.otel.collect = "reactivity"), {
    task <- ex_task_42()
  })

  # Capture the initial state
  expect_true(task$.__enclos_env__$private$is_recording_otel)

  # Change the option after initialization
  withr::with_options(list(shiny.otel.collect = "none"), {
    # The task should still have the init-time setting
    expect_true(task$.__enclos_env__$private$is_recording_otel)
  })

})

test_that("ExtendedTask respects session level otel collection", {
  # Test that session level doesn't enable reactivity spans
  withr::local_options(list(shiny.otel.collect = "session"))

  task <- ex_task_42()

  # Should not record otel at session level
  expect_false(task$.__enclos_env__$private$is_recording_otel)
})

test_that("ExtendedTask respects reactive_update level otel collection", {
  # Test that reactive_update level doesn't enable reactivity spans
  withr::local_options(list(shiny.otel.collect = "reactive_update"))

  task <- ex_task_42()

  # Should not record otel at reactive_update level
  expect_false(task$.__enclos_env__$private$is_recording_otel)
})

test_that("ExtendedTask creates span only when is_recording_otel is TRUE", {
  # Test that span is only created when otel is enabled
  withr::local_options(list(shiny.otel.collect = "reactivity"))

  span_created <- FALSE

  local_mocked_bindings(
    start_otel_span = function(...) {
      span_created <<- TRUE
      create_mock_otel_span("extended_task")
    },
    otel_is_tracing_enabled = function() TRUE
  )

  ignore <- otelsdk::with_otel_record({
    withReactiveDomain(MockShinySession$new(), {
      task <- ex_task_42()

      # Reset the flag
      span_created <- FALSE

      # Invoke the task
      isolate({
        task$invoke()
      })
    })
  })


  # Span should have been created because is_recording_otel is TRUE
  expect_true(span_created)
})

test_that("ExtendedTask does not create span when is_recording_otel is FALSE", {
  # Test that span is not created when otel is disabled
  withr::local_options(list(shiny.otel.collect = "none"))

  span_created <- FALSE

  local_mocked_bindings(
    start_otel_span = function(...) {
      span_created <<- TRUE
      create_mock_otel_span("extended_task")
    }
  )

  withReactiveDomain(MockShinySession$new(), {
    task <- ex_task_42()

    # Invoke the task
    isolate({
      task$invoke()
    })
  })

  # Span should not have been created because is_recording_otel is FALSE
  expect_false(span_created)
})


test_that("Multiple ExtendedTask invocations use same is_recording_otel value", {
  # Enable otel tracing
  withr::local_options(list(shiny.otel.collect = "reactivity"))
  local_mocked_bindings(
    otel_is_tracing_enabled = function() TRUE
  )

  withReactiveDomain(MockShinySession$new(), {
    task <- ex_task_42()

    # Verify is_recording_otel is TRUE at init
    expect_true(task$.__enclos_env__$private$is_recording_otel)

    # Change option after initialization (should not affect the task)
    withr::with_options(
      list(shiny.otel.collect = "none"),
      {
        # The task should still have the init-time setting
        expect_true(task$.__enclos_env__$private$is_recording_otel)

        # Verify is_recording_otel doesn't change on invocation
        isolate({
          task$invoke()
        })

        # Still should be TRUE after invoke
        expect_true(task$.__enclos_env__$private$is_recording_otel)
      }
    )
  })
})
