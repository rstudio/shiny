context("testModule")

library(promises)
library(future)
plan(multisession)

test_that("testModule passes dots", {
  module <- function(input, output, session, someArg) {
    expect_false(missing(someArg))
    expect_equal(someArg, 123)
  }
  testModule(module, {}, someArg = 123)
})

test_that("testModule passes dynamic dots", {
  module <- function(input, output, session, someArg) {
    expect_false(missing(someArg))
    expect_equal(someArg, 123)
  }

  # Test with !!! to splice in a whole named list constructed with base::list()
  moreArgs <- list(someArg = 123)
  testModule(module, {}, !!!moreArgs)

  # Test with !!/:= to splice in an argument name
  argName <- "someArg"
  testModule(module, {}, !!argName := 123)
})

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
    session$setInputs(x=1)
    expect_equal(rv$y, 2)
    expect_equal(rv$x, 2)
    expect_equal(output$txt, "Value: 2")

    session$setInputs(x=2)
    expect_equal(rv$x, 4)
    expect_equal(rv$y, 4)
    expect_equal(output$txt, "Value: 4")
  })
})

test_that("inputs aren't directly assignable", {
  module <- function(input, output, session) {
  }

  testModule(module, {
    session$setInputs(x = 0)
    expect_error({ input$x <- 1 }, "Attempted to assign value to a read-only")
    expect_error({ input$y <- 1 }, "Attempted to assign value to a read-only")
  })
})

test_that("testModule handles more complex expressions", {
  module <- function(input, output, session){
    output$txt <- renderText({
      input$x
    })
  }

  testModule(module, {
    for (i in 1:5){
      session$setInputs(x=i)
      expect_equal(output$txt, as.character(i))
    }
    expect_equal(output$txt, "5")

    if(TRUE){
      session$setInputs(x="abc")
      expect_equal(output$txt, "abc")
    }
  })
})

test_that("testModule handles reactiveVal", {
  module <- function(input, output, session) {
    x <- reactiveVal(0)
    observe({
      x(input$y + input$z)
    })
  }

  testModule(module, {
    session$setInputs(y=1, z=2)

    expect_equal(x(), 3)

    session$setInputs(z=3)
    expect_equal(x(), 4)

    session$setInputs(y=5)
    expect_equal(x(), 8)
  })
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
    session$setInputs(a=1, b=2, c=3)
    expect_equal(r(), 4)
    expect_equal(r2(), 7)

    session$setInputs(a=2)
    expect_equal(r(), 5)
    expect_equal(r2(), 8)

    session$setInputs(b=0)
    expect_equal(r2(), 6)
    expect_equal(r(), 3)

    session$setInputs(c=4)
    expect_equal(r(), 3)
    expect_equal(r2(), 7)
  })
})

test_that("testModule handles reactivePoll", {
  module <- function(input, output, session) {
    rv <- reactiveValues(x = 0)
    rp <- reactivePoll(50, session, function(){ rnorm(1) }, function(){
      isolate(rv$x <- rv$x + 1)
      rnorm(1)
    })

    observe({rp()})
  }

  testModule(module, {
    expect_equal(rv$x, 1)

    for (i in 1:4){
      session$elapse(50)
    }

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

    session$elapse(200)

    expect_equal(rv$x, 5)
  })
})

test_that("testModule handles debounce/throttle", {
  module <- function(input, output, session) {
    rv <- reactiveValues(t = 0, d = 0)
    react <- reactive({
      input$y
    })
    rt <- throttle(react, 100)
    rd <- debounce(react, 100)

    observe({
      rt() # Invalidate this block on the timer
      isolate(rv$t <- rv$t + 1)
    })

    observe({
      rd()
      isolate(rv$d <- rv$d + 1)
    })
  }

  testModule(module, {
    session$setInputs(y = TRUE)
    expect_equal(rv$d, 1)
    for (i in 2:5){
      session$setInputs(y = FALSE)
      session$elapse(51)
      session$setInputs(y = TRUE)
      expect_equal(rv$t, i-1)
      session$elapse(51) # TODO: we usually don't have to pad by a ms, but here we do. Investigate.
      expect_equal(rv$t, i)
    }
    # Never sufficient time to debounce. Not incremented
    expect_equal(rv$d, 1)
    session$elapse(50)

    # Now that 100ms has passed since the last update, debounce should have triggered
    expect_equal(rv$d, 2)
  })
})

test_that("testModule wraps output in an observer", {
  testthat::skip("I'm not sure of a great way to test this without timers.")
  # And honestly it's so foundational in what we're doing now that it might not be necessary to test?


  module <- function(input, output, session) {
    rv <- reactiveValues(x=0)
    rp <- reactiveTimer(50)
    output$txt <- renderText({
      rp()
      isolate(rv$x <- rv$x + 1)
    })
  }

  testModule(module, {
    session$setInputs(x=1)
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
  })

  # FIXME:
  #  - Do we want the output to be accessible natively, or some $get() on the output? If we do a get() we could
  #    do more helpful spy-type things around exec count.
  #  - plots and such?
})

test_that("testModule works with async", {
  module <- function(input, output, session) {
    output$txt <- renderText({
      val <- input$x
      future({ val })
    })

    output$error <- renderText({
      future({ stop("error here") })
    })

    output$sync <- renderText({
      # No promises here
      "abc"
    })
  }

  testModule(module, {
    session$setInputs(x=1)
    expect_equal(output$txt, "1")
    expect_equal(output$sync, "abc")

    # Error gets thrown repeatedly
    expect_error(output$error, "error here")
    expect_error(output$error, "error here")

    # Responds reactively
    session$setInputs(x=2)
    expect_equal(output$txt, "2")
    # Error still thrown
    expect_error(output$error, "error here")
  })
})

test_that("testModule works with multiple promises in parallel", {
  module <- function(input, output, session) {
    output$txt1 <- renderText({
      future({
        Sys.sleep(1)
        1
      })
    })

    output$txt2 <- renderText({
      future({
        Sys.sleep(1)
        2
      })
    })
  }

  testModule(module, {
    # As we enter this test code, the promises will still be running in the background.
    # We'll need to give them ~2s (plus overhead) to complete
    startMS <- as.numeric(Sys.time()) * 1000
    expect_equal(output$txt1, "1") # This first call will block waiting for the promise to return
    expect_equal(output$txt2, "2")
    expect_equal(output$txt2, "2") # Now that we have the values, access should not incur a 1s delay.
    expect_equal(output$txt1, "1")
    expect_equal(output$txt1, "1")
    expect_equal(output$txt2, "2")
    endMS <- as.numeric(Sys.time()) * 1000

    # We'll pad quite a bit because promises can introduce some lag. But the point we're trying
    # to prove is that we're not hitting a 1s delay for each output access, which = 6000ms. If we're
    # under that, then things are likely working.
    expect_lt(endMS - startMS, 4000)
  })
})

test_that("testModule handles async errors", {
  module <- function(input, output, session, arg1, arg2){
    output$err <- renderText({
      future({ "my error"}) %...>%
        stop() %...>%
        print() # Extra steps after the error
    })

    output$safe <- renderText({
      future({ safeError("my safe error") }) %...>%
        stop()
    })
  }

  testModule(module, {
    expect_error(output$err, "my error")
    # TODO: helper for safe errors so users don't have to learn "shiny.custom.error"?
    expect_error(output$safe, "my safe error", class="shiny.custom.error")
  })
})

test_that("testModule handles modules with additional arguments", {
  module <- function(input, output, session, arg1, arg2){
    output$txt1 <- renderText({
      arg1
    })

    output$txt2 <- renderText({
      arg2
    })

    output$inp <- renderText({
      input$x
    })
  }

  testModule(module, {
    expect_equal(output$txt1, "val1")
    expect_equal(output$txt2, "val2")
  }, arg1="val1", arg2="val2")
})

test_that("testModule captures htmlwidgets", {
  # TODO: use a simple built-in htmlwidget instead of something complex like dygraph
  if (!requireNamespace("dygraphs")){
    testthat::skip("dygraphs not available to test htmlwidgets")
  }

  if (!requireNamespace("jsonlite")){
    testthat::skip("jsonlite not available to test htmlwidgets")
  }

  module <- function(input, output, session){
    output$dy <- dygraphs::renderDygraph({
      dygraphs::dygraph(data.frame(outcome=0:5, year=2000:2005))
    })
  }

  testModule(module, {
    # Really, this test should be specific to each htmlwidget. Here, we don't want to bind ourselves
    # to the current JSON structure of dygraphs, so we'll just check one element to see that the raw
    # JSON was exposed and is accessible in tests.
    d <- jsonlite::fromJSON(output$dy)$x$data
    expect_equal(d[1,], 0:5)
    expect_equal(d[2,], 2000:2005)
  })
})

test_that("testModule captures renderUI", {
  module <- function(input, output, session){
    output$ui <- renderUI({
      tags$a(href="https://rstudio.com", "hello!")
    })
  }

  testModule(module, {
    expect_equal(output$ui$deps, list())
    expect_equal(as.character(output$ui$html), "<a href=\"https://rstudio.com\">hello!</a>")
  })
})

test_that("testModule captures base graphics outputs", {
  module <- function(input, output, session){
    output$fixed <- renderPlot({
      plot(1,1)
    }, width=300, height=350)

    output$dynamic <- renderPlot({
      plot(1,1)
    })
  }

  testModule(module, {
    # We aren't yet able to create reproducible graphics, so this test is intentionally pretty
    # limited.
    expect_equal(output$fixed$width, 300)
    expect_equal(output$fixed$height, 350)
    expect_match(output$fixed$src, "^data:image/png;base64,")

    # Ensure that the plot defaults to a reasonable size.
    expect_equal(output$dynamic$width, 600)
    expect_equal(output$dynamic$height, 400)
    expect_match(output$dynamic$src, "^data:image/png;base64,")

    # TODO: how do you customize automatically inferred plot sizes?
    # session$setPlotMeta("dynamic", width=600, height=300) ?
  })
})

test_that("testModule captures ggplot2 outputs", {
  if (!requireNamespace("ggplot2")){
    testthat::skip("ggplot2 not available")
  }

  module <- function(input, output, session){
    output$fixed <- renderPlot({
      ggplot2::qplot(iris$Sepal.Length, iris$Sepal.Width)
    }, width=300, height=350)

    output$dynamic <- renderPlot({
      ggplot2::qplot(iris$Sepal.Length, iris$Sepal.Width)
    })
  }

  testModule(module, {
    expect_equal(output$fixed$width, 300)
    expect_equal(output$fixed$height, 350)
    expect_match(output$fixed$src, "^data:image/png;base64,")

    # Ensure that the plot defaults to a reasonable size.
    expect_equal(output$dynamic$width, 600)
    expect_equal(output$dynamic$height, 400)
    expect_match(output$dynamic$src, "^data:image/png;base64,")
  })
})

test_that("testModule exposes the returned value from the module", {
  module <- function(input, output, session){
    reactive({
      return(input$a + input$b)
    })
  }

  testModule(module, {
    session$setInputs(a=1, b=2)
    expect_equal(session$returned(), 3)

    # And retains reactivity
    session$setInputs(a=2)
    expect_equal(session$returned(), 4)
  })
})

test_that("testModule handles synchronous errors", {
  module <- function(input, output, session, arg1, arg2){
    output$err <- renderText({
      stop("my error")
    })

    output$safe <- renderText({
      stop(safeError("my safe error"))
    })
  }

  testModule(module, {
    expect_error(output$err, "my error")
    # TODO: helper for safe errors so users don't have to learn "shiny.custom.error"?
    expect_error(output$safe, "my safe error", class="shiny.custom.error")
  })
})

test_that("accessing a non-existant output gives an informative message", {
  module <- function(input, output, session){}

  testModule(module, {
    expect_error(output$dontexist, "hasn't been defined yet: output\\$dontexist")
  })
})

test_that("testModule works with nested modules", {
  outerModule <- function(input, output, session) {
    r1 <- reactive({ input$x + 1})
    r2 <- callModule(innerModule, "innerModule", r1)
    output$someVar <- renderText(r2())
  }

  innerModule <- function(input, output, session, r) {
    reactive(paste("a value:", r()))
  }

  testModule(outerModule, {
    session$setInputs(x = 1)
    expect_equal(output$someVar, "a value: 2")
  })
})

test_that("assigning an output in a module function with a non-function errors", {
  module <- function(input, output, session) {
    output$someVar <- 123
  }

  expect_error(testModule(module, {}), "^Unexpected")
})

test_that("testServer works", {
  # app.R
  testServer({
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  }, appDir=test_path("..", "test-modules", "06_tabsets"))

  # server.R
  testServer({
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  }, appDir=test_path("..", "test-modules", "server_r"))
})

test_that("testServer works when referencing external globals", {
  # If global is defined at the top of app.R outside of the server function.
  testServer({
    expect_equal(global, 123)
  }, appDir=test_path("..", "test-modules", "06_tabsets"))
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

    session$elapse(49)
    expect_equal(rv$x, 1)

    session$elapse(1)
    # Should have been incremented now
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
    session$setInputs(x=1)
    expect_equal(rv$x, 2)
    # We're not concerned with the exact values here -- only that they increase
    fc <- rv$flushCounter
    fdc <- rv$flushedCounter

    session$setInputs(x=2)
    expect_gt(rv$flushCounter, fc)
    expect_gt(rv$flushedCounter, fdc)

    # These should have only run once
    expect_equal(rv$flushOnceCounter, 1)
    expect_equal(rv$flushedOnceCounter, 1)

  })
})

test_that("findApp errors with no app", {
  calls <- 0
  nothingExists <- function(path){
    calls <<- calls + 1
    FALSE
  }
  fa <- rewire(findApp, file.exists.ci=nothingExists)
  expect_error(
    expect_warning(fa("/some/path/here"), "No such file or directory"), # since we just made up a path
    "No shiny app was found in ")
  expect_equal(calls, 4 * 2) # Checks here, path, some, and / -- looking for app.R and server.R for each
})

test_that("findApp works with app in current or parent dir", {
  calls <- 0
  cd <- normalizePath(".")
  mockExists <- function(path){
    # Only TRUE if looking for server.R or app.R in current Dir
    calls <<- calls + 1

    path <- normalizePath(path, mustWork = FALSE)

    appPath <- normalizePath(file.path(cd, "app.R"), mustWork = FALSE)
    serverPath <- normalizePath(file.path(cd, "server.R"), mustWork = FALSE)
    return(path %in% c(appPath, serverPath))
  }
  fa <- rewire(findApp, file.exists.ci=mockExists)
  expect_equal(fa(), cd)
  expect_equal(calls, 1) # Should get a hit on the first call and stop

  # Reset and point to the parent dir
  calls <- 0
  cd <- normalizePath("..") # TODO: won't work if running tests in the root dir.
  f <- fa()
  expect_equal(normalizePath(f, mustWork = FALSE), cd)
  expect_equal(calls, 3) # Two for current dir and hit on the first in the parent
})
