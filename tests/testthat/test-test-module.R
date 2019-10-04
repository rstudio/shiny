context("testModule")

test_that("testModule handles basic reactivity", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    observe({
      rv$x <- input$x * 2
    })
    output$txt <- renderText({
      paste0("Value: ", rv$x)
    })
  }

  testModule(module, {
    expect_equal(rv$x, 2)
    expect_equal(output$txt(), "Value: 2")

    input$x <- 2
    expect_equal(rv$x, 4)
    expect_equal(output$txt(), "Value: 4")
  }, initialState = list(x=1))
})

test_that("testModule handles timers", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    observe({
      isolate(rv$x <- rv$x + 1)
      # We're only testing one invalidation
      if (isolate(rv$x) <= 1){
        invalidateLater(50, NULL)
      }
    })
  }

  testModule(module, {
    # Should have run once
    expect_equal(rv$x, 1)

    now <- as.numeric(Sys.time()) * 1000
    # wait 80 milliseconds which should give the invalidateLater time to run
    Sys.sleep(0.08)
    # FIXME: I don't like running this myself because it pollutes the code under
    # test. Is there a better way to sleep while not blocking the event loop?
    later::run_now()

    # Should have been incremented again by now.
    expect_equal(rv$x, 2)
  })
})

test_that("session flush handlers work", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0, flushCounter = 0, flushedCounter = 0,
                         flushOnceCounter = 0, flushedOnceCounter = 0)

    onFlush(function(){rv$flushCounter <- rv$flushCounter + 1}, once=FALSE, session)
    onFlushed(function(){rv$flushedCounter <- rv$flushedCounter + 1}, once=FALSE, session)
    onFlushed(function(){rv$flushOnceCounter <- rv$flushOnceCounter + 1}, once=TRUE, session)
    onFlushed(function(){rv$flushedOnceCounter <- rv$flushedOnceCounter + 1}, once=TRUE, session)

    observe({
      rv$x <- input$x * 2
    })
  }

  testModule(module, {
    expect_equal(rv$x, 2)
    # We're not concerned with the exact values here -- only that they increase
    fc <- rv$flushCounter
    fdc <- rv$flushedCounter

    input$x <- 2
    expect_gt(rv$flushCounter, fc)
    expect_gt(rv$flushedCounter, fdc)

    # These should have only run once
    expect_equal(rv$flushOnceCounter, 1)
    expect_equal(rv$flushedOnceCounter, 1)

  }, initialState = list(x=1))
})
