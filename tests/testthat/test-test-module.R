context("testModule")

test_that("testModule handles observers", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0, y = 0)
    observe({
      rv$x <- input$x * 2
    })
    observe({
      rv$y <- rv$x
    })
    output$txt <- renderText({
      paste0("Value: ", rv$x)
    })
  }

  testModule(module, {
    expect_equal(rv$y, 2)
    expect_equal(rv$x, 2)
    expect_equal(output$txt(), "Value: 2")

    input$x <- 2
    expect_equal(rv$x, 4)
    expect_equal(rv$y, 4)
    expect_equal(output$txt(), "Value: 4")
  }, initialState = list(x=1))
})

test_that("testModule handles reactiveVal", {
  module <- function(input, output, session) {
    x <- reactiveVal(0)
    observe({
      x(input$y + input$z)
    })
  }

  testModule(module, {
    expect_equal(x(), 3)

    input$z <- 3
    expect_equal(x(), 4)

    input$y <- 5
    expect_equal(x(), 8)
  }, initialState = list(y=1, z=2))
})

test_that("testModule handles reactives with complex dependency tree", {
  module <- function(input, output, session) {
    x <- reactiveValues(x=1)
    r <- reactive({
      x$x + input$a + input$b
    })
    r2 <- reactive({
      r() + input$c
    })
  }

  testModule(module, {
    expect_equal(r(), 4)
    expect_equal(r2(), 7)

    input$a <- 2
    expect_equal(r(), 5)
    expect_equal(r2(), 8)

    input$b <- 0
    expect_equal(r2(), 6)
    expect_equal(r(), 3)

    input$c <- 4
    expect_equal(r(), 3)
    expect_equal(r2(), 7)
  }, initialState = list(a=1, b=2, c=3))
})

test_that("testModule handles reactivePoll", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    rp <- reactivePoll(50, session, function(){ as.numeric(Sys.time()) }, function(){
      isolate(rv$x <- rv$x + 1)
      as.numeric(Sys.time())
    })

    observe({rp()})
  }

  testModule(module, {
    expect_equal(rv$x, 1)

    # Have to repeat these on separate lines because we only give `flushReact` a
    # chance to run in between each line of this expression
    Sys.sleep(.055) # Pad the sleep a bit to accommodate compute overhead
    Sys.sleep(.055)
    Sys.sleep(.055)
    Sys.sleep(.055)

    expect_equal(rv$x, 5)
  })
})

test_that("testModule handles reactiveTimer", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)

    rp <- reactiveTimer(50)
    observe({
      rp()
      isolate(rv$x <- rv$x + 1)
    })
  }

  testModule(module, {
    expect_equal(rv$x, 1)

    # Have to repeat these on separate lines because we only give `flushReact` a
    # chance to run in between each line of this expression.
    Sys.sleep(.055) # pad the timer interval a bit to accommodate compute overhead
    Sys.sleep(.055)
    Sys.sleep(.055)
    Sys.sleep(.055)

    expect_equal(rv$x, 5)
  })
})

test_that("testModule handles debounce/throttle", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    rp <- throttle(reactiveTimer(50), 300)

    observe({
      rp() # Invalidate this block on the timer
      isolate(rv$x <- rv$x + 1)
    })
  }

  testModule(module, {
    expect_equal(rv$x, 1)

    # Have to repeat these on separate lines because we only give `flushReact` a
    # chance to run in between each line of this expression.
    Sys.sleep(.15) # Won't be ready to fire yet
    Sys.sleep(.15) # Now should have one
    Sys.sleep(.15) # Not yet
    expect_equal(rv$x, 2)
  })
})

test_that("testModule handles rendering output correctly", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x=0)
    rp <- reactiveTimer(50)
    output$txt <- renderText({
      rp()
      isolate(rv$x <- rv$x + 1)
    })
  }

  testModule(module, {
    # Timers only tick if they're being observed. If the output weren't being
    # wrapped in an observer, we'd see the value of rv$x initialize to zero and
    # only increment when we evaluated the output. e.g.:
    #
    # expect_equal(rv$x, 0)
    # Sys.sleep(1)
    # expect_equal(rv$x, 0)
    # output$txt()
    # expect_equal(rv$x, 1)

    expect_equal(rv$x, 1)
    expect_equal(output$txt, "1")
    Sys.sleep(.05)
    Sys.sleep(.05)
    expect_gt(rv$x, 1)
    expect_equal(output$txt, as.character(rv$x))
  }, initialState = list(x=1))

  # FIXME:
  #  - Do we want the output to be accessible natively, or some $get() on the output? If we do a get() we could
  #    do more helpful spy-type things around exec count.
  #  - plots and such?
})

test_that("testServer works", {
  testthat::skip("NYI")
})

test_that("testModule handles invalidateLater", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    observe({
      isolate(rv$x <- rv$x + 1)
      # We're only testing one invalidation
      if (isolate(rv$x) <= 1){
        invalidateLater(50)
      }
    })
  }

  testModule(module, {
    # Should have run once
    expect_equal(rv$x, 1)

    # now <- as.numeric(Sys.time()) * 1000
    # wait 80 milliseconds which should give the invalidateLater time to run
    Sys.sleep(0.08)
    # FIXME: I don't like running this myself because it pollutes the code under
    # test. Is there a better way to sleep while not blocking the event loop?

    # Should have been incremented again by now.
    expect_equal(rv$x, 2)
  })
})

test_that("session ended handlers work", {
  module <- function(input, output, session){}

  testModule(module, {
    rv <- reactiveValues(closed = FALSE)
    session$onEnded(function(){
      rv$closed <- TRUE
    })

    expect_equal(session$isEnded(), FALSE)
    expect_equal(session$isClosed(), FALSE)
    expect_false(rv$closed, FALSE)

    session$close()

    expect_equal(session$isEnded(), TRUE)
    expect_equal(session$isClosed(), TRUE)
    expect_false(rv$closed, TRUE)
  })
})

test_that("session flush handlers work", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0, flushCounter = 0, flushedCounter = 0,
                         flushOnceCounter = 0, flushedOnceCounter = 0)

    onFlush(function(){rv$flushCounter <- rv$flushCounter + 1}, once=FALSE)
    onFlushed(function(){rv$flushedCounter <- rv$flushedCounter + 1}, once=FALSE)
    onFlushed(function(){rv$flushOnceCounter <- rv$flushOnceCounter + 1}, once=TRUE)
    onFlushed(function(){rv$flushedOnceCounter <- rv$flushedOnceCounter + 1}, once=TRUE)

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
