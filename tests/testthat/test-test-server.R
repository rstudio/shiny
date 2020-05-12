context("testServer")

library(shiny)
library(testthat)
library(future)
library(promises)

test_that("testServer passes dots", {
  module <- function(id, someArg) {
    expect_false(missing(someArg))
    moduleServer(id, function(input, output, session) {
      expect_equal(someArg, 123)
    })
  }
  testServer(module, {}, args = list(someArg = 123))
})

test_that("testServer handles observers", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
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
    })
  }

  testServer(module, {
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
  module <- function(id) moduleServer(id, function(input, output, session) {})

  testServer(module, {
    session$setInputs(x = 0)
    expect_error({ input$x <- 1 })
    expect_error({ input$y <- 1 })
  })
})

test_that("setInputs dots are dynamic", {
  module <- function(id) moduleServer(id, function(input, output, session) {})

  inputs_initial <- list(x=1, y=2)
  input_y <- "y"

  testServer(module, {
    session$setInputs(!!!inputs_initial)
    expect_equal(input$x, 1)
    expect_equal(input$y, 2)
    session$setInputs(!!input_y := 3)
    expect_equal(input$y, 3)

    # Duplicate names are an error
    expect_error(session$setInputs(x = 1, x = 2))
  })
})

test_that("testServer handles more complex expressions", {
  module <- function(id) {
    moduleServer(id, function(input, output, session){
      output$txt <- renderText({
        input$x
      })
    })
  }

  testServer(module, {
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

test_that("testServer handles reactiveVal", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      x <- reactiveVal(0)
      observe({
        x(input$y + input$z)
      })
    })
  }

  testServer(module, {
    session$setInputs(y=1, z=2)

    expect_equal(x(), 3)

    session$setInputs(z=3)
    expect_equal(x(), 4)

    session$setInputs(y=5)
    expect_equal(x(), 8)
  })
})

test_that("testServer handles reactives with complex dependency tree", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      x <- reactiveValues(x=1)
      r <- reactive({
        x$x + input$a + input$b
      })
      r2 <- reactive({
        r() + input$c
      })
    })
  }

  testServer(module, {
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

test_that("testServer handles reactivePoll", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveValues(x = 0)
      rp <- reactivePoll(50, session, function(){ rnorm(1) }, function(){
        isolate(rv$x <- rv$x + 1)
        rnorm(1)
      })

      observe({rp()})
    })
  }

  testServer(module, {
    session$flushReact()
    expect_equal(rv$x, 1)

    for (i in 1:4){
      session$elapse(50)
    }

    expect_equal(rv$x, 5)
  })
})

test_that("testServer handles reactiveTimer", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveValues(x = 0)

      rp <- reactiveTimer(50)
      observe({
        rp()
        isolate(rv$x <- rv$x + 1)
      })
    })
  }

  testServer(module, {
    session$flushReact()
    expect_equal(rv$x, 1)

    session$elapse(200)

    expect_equal(rv$x, 5)
  })
})

test_that("testServer handles debounce/throttle", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
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
    })
  }

  testServer(module, {
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

test_that("testServer wraps output in an observer", {
  testthat::skip("I'm not sure of a great way to test this without timers.")
  # And honestly it's so foundational in what we're doing now that it might not be necessary to test?

  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveValues(x=0)
      rp <- reactiveTimer(50)
      output$txt <- renderText({
        rp()
        isolate(rv$x <- rv$x + 1)
      })
    })
  }

  testServer(module, {
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

test_that("testServer works with async", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
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
    })
  }

  testServer(module, {
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
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
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
    })
  }

  testServer(module, {
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
  module <- function(id) {
    moduleServer(id, function(input, output, session, arg1, arg2){
      output$err <- renderText({
        future({ "my error"}) %...>%
          stop() %...>%
          print() # Extra steps after the error
      })

      output$safe <- renderText({
        future({ safeError("my safe error") }) %...>%
          stop()
      })
    })
  }

  testServer(module, {
    expect_error(output$err, "my error")
    # TODO: helper for safe errors so users don't have to learn "shiny.custom.error"?
    expect_error(output$safe, "my safe error", class="shiny.custom.error")
  })
})

test_that("testServer handles modules with additional arguments", {
  module <- function(id, arg1, arg2) {
    moduleServer(id, function(input, output, session){
      output$txt1 <- renderText({
        arg1
      })

      output$txt2 <- renderText({
        arg2
      })

      output$inp <- renderText({
        input$x
      })
    })
  }

  testServer(module, {
    expect_equal(output$txt1, "val1")
    expect_equal(output$txt2, "val2")
  }, list(arg1="val1", arg2="val2"))
})

test_that("testServer captures htmlwidgets", {
  # TODO: use a simple built-in htmlwidget instead of something complex like dygraph
  if (!requireNamespace("dygraphs")){
    testthat::skip("dygraphs not available to test htmlwidgets")
  }

  if (!requireNamespace("jsonlite")){
    testthat::skip("jsonlite not available to test htmlwidgets")
  }

  module <- function(id) {
    moduleServer(id, function(input, output, session){
      output$dy <- dygraphs::renderDygraph({
        dygraphs::dygraph(data.frame(outcome=0:5, year=2000:2005))
      })
    })
  }

  testServer(module, {
    # Really, this test should be specific to each htmlwidget. Here, we don't want to bind ourselves
    # to the current JSON structure of dygraphs, so we'll just check one element to see that the raw
    # JSON was exposed and is accessible in tests.
    d <- jsonlite::fromJSON(output$dy)$x$data
    expect_equal(d[1,], 0:5)
    expect_equal(d[2,], 2000:2005)
  })
})

test_that("testServer captures renderUI", {
  module <- function(id) {
    moduleServer(id, function(input, output, session){
      output$ui <- renderUI({
        tags$a(href="https://rstudio.com", "hello!")
      })
    })
  }

  testServer(module, {
    expect_equal(output$ui$deps, list())
    expect_equal(as.character(output$ui$html), "<a href=\"https://rstudio.com\">hello!</a>")
  })
})

test_that("testServer captures base graphics outputs", {
  module <- function(id) {
    moduleServer(id, function(input, output, session){
      output$fixed <- renderPlot({
        plot(1,1)
      }, width=300, height=350)

      output$dynamic <- renderPlot({
        plot(1,1)
      })
    })
  }

  testServer(module, {
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

test_that("testServer captures ggplot2 outputs", {
  if (!requireNamespace("ggplot2")){
    testthat::skip("ggplot2 not available")
  }

  module <- function(id) {
    moduleServer(id, function(input, output, session){
      output$fixed <- renderPlot({
        ggplot2::qplot(iris$Sepal.Length, iris$Sepal.Width)
      }, width=300, height=350)

      output$dynamic <- renderPlot({
        ggplot2::qplot(iris$Sepal.Length, iris$Sepal.Width)
      })
    })
  }

  testServer(module, {
    expect_equal(output$fixed$width, 300)
    expect_equal(output$fixed$height, 350)
    expect_match(output$fixed$src, "^data:image/png;base64,")

    # Ensure that the plot defaults to a reasonable size.
    expect_equal(output$dynamic$width, 600)
    expect_equal(output$dynamic$height, 400)
    expect_match(output$dynamic$src, "^data:image/png;base64,")
  })
})

test_that("testServer exposes the returned value from the module", {
  module <- function(id) {
    moduleServer(id, function(input, output, session){
      reactive({
        return(input$a + input$b)
      })
    })
  }

  testServer(module, {
    session$setInputs(a=1, b=2)
    expect_equal(session$getReturned()(), 3)

    # And retains reactivity
    session$setInputs(a=2)
    expect_equal(session$getReturned()(), 4)
  })
})

test_that("testServer handles synchronous errors", {
  module <- function(id) {
    moduleServer(id, function(input, output, session, arg1, arg2){
      output$err <- renderText({
        stop("my error")
      })

      output$safe <- renderText({
        stop(safeError("my safe error"))
      })
    })
  }

  testServer(module, {
    expect_error(output$err, "my error")
    # TODO: helper for safe errors so users don't have to learn "shiny.custom.error"?
    expect_error(output$safe, "my safe error", class="shiny.custom.error")
  })
})

test_that("accessing a non-existent output gives an informative message", {
  module <- function(id) {
    moduleServer(id, function(input, output, session){})
  }

  testServer(module, {
    expect_error(output$dontexist, "hasn't been defined yet: output\\$server1-dontexist")
  }, list(id = "server1"))

  testServer(module, {
    expect_error(output$dontexist, "hasn't been defined yet: output\\$.*-dontexist")
  })
})

test_that("assigning an output in a module function with a non-function errors", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$someVar <- 123

    })
  }

  expect_error(testServer(module, {}), "^Unexpected")
})

test_that("testServer handles invalidateLater", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveValues(x = 0)
      observe({
        isolate(rv$x <- rv$x + 1)
        # We're only testing one invalidation
        if (isolate(rv$x) <= 1){
          invalidateLater(50)
        }
      })
    })
  }

  testServer(module, {
    session$flushReact()
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
  module <- function(id) {
    moduleServer(id, function(input, output, session){})
  }

  testServer(module, {
    rv <- reactiveValues(closed = FALSE)
    session$onEnded(function(){
      rv$closed <- TRUE
    })

    expect_equal(session$isEnded(), FALSE)
    expect_equal(session$isClosed(), FALSE)
    expect_false(rv$closed)

    session$close()

    expect_equal(session$isEnded(), TRUE)
    expect_equal(session$isClosed(), TRUE)
    expect_true(rv$closed)
  })
})

test_that("session flush handlers work", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      rv <- reactiveValues(x = 0, flushCounter = 0, flushedCounter = 0,
                           flushOnceCounter = 0, flushedOnceCounter = 0)

      onFlush(function(){rv$flushCounter <- rv$flushCounter + 1}, once=FALSE)
      onFlushed(function(){rv$flushedCounter <- rv$flushedCounter + 1}, once=FALSE)
      onFlushed(function(){rv$flushOnceCounter <- rv$flushOnceCounter + 1}, once=TRUE)
      onFlushed(function(){rv$flushedOnceCounter <- rv$flushedOnceCounter + 1}, once=TRUE)

      observe({
        rv$x <- input$x * 2
      })
    })
  }

  testServer(module, {
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

test_that("module return value captured", {
  module_implicit_return <- function(id) {
    moduleServer(id, function(input, output, session) {
      123
    })
  }

  testServer(module_implicit_return, {
    expect_equal(session$returned, 123)
  })

  module_early_returns <- function(id, n) {
    retval <- NULL
    moduleServer(id, function(input, output, session) {
      if (n == 0) return(n)
      if (n %% 2 == 0) {
        retval <<- "even"
      } else {
        return(FALSE)
      }
      retval
    })
  }

  testServer(module_early_returns, {
    expect_equal(session$returned, 0)
  }, args = list(n = 0))

  testServer(module_early_returns, {
    expect_equal(session$returned, FALSE)
  }, args = list(n = 1))

  testServer(module_early_returns, {
    expect_equal(session$returned, "even")
  }, args = list(n = 2))
})

test_that("It's an error to pass arguments to a server", {
  expect_error(testServer(test_path("..", "test-modules", "06_tabsets"), {}, args = list(an_arg = 123)))
})

# Provided an instance of an R6 object and its generator, returns a list with
# `methods` and `fields`. `methods` contains a character vector of names of
# public methods. `fields` is a character vector of public fields. Any active
# bindings are considered `fields`.
get_mocked_publics <- function(instance, generator) {
  publics <- ls(instance, all.names = TRUE)
  actives <- names(generator$active) %OR% character(0)
  # Active bindings are considered fields.
  methods_or_fields <- publics[!(publics %in% actives)]
  methods <- character(0)
  fields <- actives
  for (name in methods_or_fields) {
    if (is.function(instance[[name]])) {
      methods <- c(methods, name)
    } else {
      fields <- c(fields, name)
    }
  }
  list(methods = methods, fields = fields)
}

test_that("MockShinySession has all public ShinySession methods and fields", {
  real_methods <- names(ShinySession$public_methods)
  real_fields <- c(names(ShinySession$public_fields), names(ShinySession$active))

  # Here we must instantiate a MockShinySession because methods are added to the
  # instance in the constructor.
  mock_session <- MockShinySession$new()
  mocked <- get_mocked_publics(mock_session, MockShinySession)

  expect_equal(intersect(real_methods, mocked$methods), real_methods)
  expect_equal(intersect(real_fields, mocked$fields), real_fields)
})

test_that("downloadHandler() works", {
  data <- mtcars
  tmpd <- NULL

  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      filename <- reactive({
        paste0(input$name, ".", input$extension)
      })
      output$downloadData <- downloadHandler(
        filename = filename(),
        content = function(file) {
          tmpd <<- dirname(file)
          saveRDS(data, file)
        }
      )
    })
  }

  testServer(module, {
    session$setInputs(name = "mtcars", extension = "rds")
    f <- output$downloadData
    expect_equal(basename(f), "mtcars.rds")
    expect_equal(readRDS(f), data)
  })

  # Ensure the temp file was closed when the session ended.
  expect_false(file.exists(tmpd))
})

test_that("getOutputInfo() returns current output name", {
  savedOutputInfo <- NULL

  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$txt <- renderText({
        savedOutputInfo <<- getCurrentOutputInfo()
        "some text"
      })
    })
  }

  testServer(module, {
    expect_equal(savedOutputInfo, NULL)
    # savedOutputInfo is not set until output$txt is accessed
    expect_equal(output$txt, "some text")
    expect_equal(savedOutputInfo, list(name = session$ns("txt")))
    expect_equal(getCurrentOutputInfo(), NULL)
  })
})

test_that("renderCachedPlot with cache = app and cache = session works", {
  module <- function(id, cache, callback) {
    moduleServer(id, function(input, output, session) {
      output$plot <- renderCachedPlot({
        callback()
        plot(input$x, input$y)
      },
        cacheKeyExpr = c(input$x, input$y),
        cache = cache
      )
    })
  }

  timesRendered <- 0
  callback <- function() (timesRendered <<- timesRendered + 1)

  testServer(module, {
    expect_equal(timesRendered, 0)
    session$setInputs(x = 1:10, y = 1:10)
    output$plot
    expect_equal(timesRendered, 1)
    session$setInputs(x = 1:10, y = 1:10)
    output$plot
    expect_equal(timesRendered, 1)
  }, args = list(cache = "session", callback = callback))

  timesRendered <- 0

  testServer(module, {
    expect_equal(timesRendered, 0)
    session$setInputs(x = 1:10, y = 1:10)
    output$plot
    expect_equal(timesRendered, 1)
    session$setInputs(x = 1:10, y = 1:10)
    output$plot
    expect_equal(timesRendered, 1)
  }, args = list(cache = "app", callback = callback))
})


test_that("promise chains evaluate in correct order", {
  messages <- list()
  clearMessages <- function() {
    messages <<- list()
  }
  pushMessage <- function(msg) {
    messages <<- c(messages, msg)
  }

  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      r1 <- reactive({
        promise(function(resolve, reject) {
          pushMessage("promise 1")
          resolve(input$go)
        })$then(function(value) {
          pushMessage(paste("promise 1 then", value))
          paste("r1", input$go)
        })
      })
      r2 <- reactive({
        promise(function(resolve, reject) {
          pushMessage("promise 2")
          resolve(input$go)
        })$then(function(value) {
          pushMessage(paste("promise 2 then", value))
          paste("r2", input$go)
        })
      })
      output$text1 <- renderText({
        pushMessage("output$text1")
        r1()
      })
      output$text2 <- renderText({
        pushMessage("output$text2")
        input$go
        r2()
      })
    })
  }

  testServer(module, {
    expect_length(messages, 0)
    session$setInputs(go = 1)
    expect_equal(output$text1, "r1 1")
    expect_equal(output$text2, "r2 1")
    expect_equal(messages, list(
      "output$text1",
      "promise 1",
      "output$text2",
      "promise 2",
      "promise 1 then 1",
      "promise 2 then 1"
    ))
    clearMessages()
    session$setInputs(go = 2)
    expect_equal(output$text1, "r1 2")
    expect_equal(output$text2, "r2 2")
    expect_equal(messages, list(
      "output$text1",
      "promise 1",
      "output$text2",
      "promise 2",
      "promise 1 then 2",
      "promise 2 then 2"
    ))
  })
})
