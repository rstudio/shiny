test_that("can access reactive values directly", {
  reactiveConsole(TRUE)
  on.exit(reactiveConsole(FALSE))

  x1 <- reactiveVal(1)
  x1(2)
  expect_equal(x1(), 2)

  x2 <- reactiveValues(a = 1)
  x2$a <- 2
  expect_equal(x2$a, 2)

  y <- reactive(x1() + x2$a)
  expect_equal(y(), 4)
})


# Test conditionalReactives ----
inputServer <- function() {
  return(function(id) {
    moduleServer(id, function(input, output, session) {
	    # counter that increments when y is fired
      i <- reactiveVal(0L)
      y <- reactive({
        i(isolate(i()) + 1)
        input$x
      })

      output$out <- renderText(y())
    })}
  )
}

test_that('Reactive updates every time its input is triggered', {
  testServer(inputServer(), {
    expect_equal(i(), 0)
    session$setInputs(x = 1)
    expect_equal(y(), 1)
    expect_equal(i(), 1)
    expect_equal(output$out, '1')

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)
    expect_equal(output$out, '2')

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)
  })

})





## Test normal updates of input -------------------------
test_that('Reactive updates every time its input is triggered', {
  testServer(inputServer(), {
    expect_equal(i(), 0)
    session$setInputs(x = 1)
    expect_equal(y(), 1)
    expect_equal(i(), 1)
    expect_equal(output$out, '1')

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)
    expect_equal(output$out, '2')

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)
  })

})

conditionalServer <- function(fire.on.NULL='never', fire.on.NA='never', echo=FALSE) {
  return(function(id) {
    moduleServer(id, function(input, output, session) {
      x <- conditionalReactive(reactive(input$x), fire.on.NULL=fire.on.NULL, fire.on.NA=fire.on.NA)

	  # counter that increments when y is fired
      i <- reactiveVal(0)
      y <- reactive({
        i(isolate(i()) + 1)
        if (echo) cat('x:', isolate(input$x), ' - x():', x(), '\n')
        x()
      })
    })}
  )
}

test_that('Conditional reactive only updates when its input changes', {
  testServer(conditionalServer(), {
    expect_equal(i(), 0)
    session$setInputs(x = 1)
    expect_equal(y(), 1)
    expect_equal(i(), 1)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)
  })
})

## Test updates with NULL -------------------------
test_that('Reactive updates every time if its input is repeatedly NULL', {
  testServer(inputServer(), {
    expect_equal(i(), 0)
    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)

    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 2)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 4)

	session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 5)
  })
})

### fire.on.NULL = 'never' -----
test_that('Conditional reactive never triggers on NULLs', {
  testServer(conditionalServer(fire.on.NULL='never'), {
    expect_equal(i(), 0)
	  expect_null(x())
    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)  ## the reactive 'y' is triggered by the initial NULL

    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	  session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	  session$setInputs(x = NULL)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	  session$setInputs(x = NULL)
    expect_equal(y(), 2)
    expect_equal(i(), 2)
  })
})


### fire.on.NULL = 'always' -----
test_that('Conditional reactive triggers on all NULLs', {
  testServer(conditionalServer(fire.on.NULL='always'), {
    expect_equal(i(), 0)
	  expect_null(x())
    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)

    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 2)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 4)

	session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 5)
  })
})

### fire.on.NULL = 'change' -----
test_that('Conditional reactive passes NULL values on to checkFun', {
  testServer(conditionalServer(fire.on.NULL='change'), {
    expect_equal(i(), 0)
	  expect_null(x())
    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)   ## the reactive 'y' is triggered by the initial NULL

    session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 1)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	  session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	  session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 3)

	session$setInputs(x = NULL)
    expect_null(y())
    expect_equal(i(), 3)
  })
})


## Test updates with NA -------------------------
test_that('Reactive updates every time if its input is repeatedly NA', {
  testServer(inputServer(), {
    expect_equal(i(), 0)
    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 1)

    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 2)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 4)

	session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 5)

	session$setInputs(x = NA_character_)
    expect_true(is.na(y()))
    expect_equal(i(), 6)
  })
})

### fire.on.NA = 'never' -----
test_that('Conditional reactive never triggers on NAs', {
  testServer(conditionalServer(fire.on.NA='never'), {
    expect_equal(i(), 0)
	expect_null(x())
    session$setInputs(x = NA)
    expect_null(y())
    expect_equal(i(), 1)  ## the reactive 'y' is triggered by the initial NA

    session$setInputs(x = NA)
    expect_null(y())
    expect_equal(i(), 1)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	session$setInputs(x = NA)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	session$setInputs(x = NA_character_)
    expect_equal(y(), 2)
    expect_equal(i(), 2)
  })
})

### fire.on.NA = 'always' -----
test_that('Conditional reactive always triggers on NAs', {
  testServer(conditionalServer(fire.on.NA='always'), {
    expect_equal(i(), 0)
	expect_null(x())
    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 1)  ## the reactive 'y' is triggered by the initial NA

    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 2)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 3)

	session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 4)

	session$setInputs(x = NA_character_)
    expect_true(is.na(y()))
    expect_equal(i(), 5)
  })
})

### fire.on.NA = 'change' -----
test_that('Conditional reactive always triggers on NAs', {
  testServer(conditionalServer(fire.on.NA='change'), {
    expect_equal(i(), 0)
	expect_null(x())
    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 1)  ## the reactive 'y' is triggered by the initial NA

    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 1)

    session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	session$setInputs(x = 2)
    expect_equal(y(), 2)
    expect_equal(i(), 2)

	session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 3)

	session$setInputs(x = NA_character_)
    expect_true(is.na(y()))
    expect_equal(i(), 3)
  })
})

## Test updates with combination of NULLs and NAs -------------------------
### fire.on.NA and fire.on.NULL = 'never'
test_that('Conditional reactive never triggers on NAs nor NULLs', {
  testServer(conditionalServer(fire.on.NULL='never',fire.on.NA='never'), {
	expect_equal(i(), 0)
	expect_null(x())
    session$setInputs(x = NA)
    expect_null(y())
    expect_equal(i(), 1, label='i() after setting "x = NA"')  ## the reactive 'y' is triggered by the initial NA/NULL

    session$setInputs(x = NULL)
	expect_null(y())
    expect_equal(i(), 1, label='i() after setting "x = NULL"')

	session$setInputs(x = NA)
	expect_null(y())
    expect_equal(i(), 1, label='i() after re-setting x back to "NA"')

	session$setInputs(x = 1)
	expect_equal(y(), 1)
    expect_equal(i(), 2)

	session$setInputs(x = 1)
	expect_equal(y(), 1)
    expect_equal(i(), 2)

	session$setInputs(x = NA)
	expect_equal(y(), 1)
    expect_equal(i(), 2, label='i() after setting "x = NA", after x was 1')

	session$setInputs(x = NULL)
	expect_equal(y(), 1)
    expect_equal(i(), 2, label='i() after setting "x = NULL",  after x was 1')

  })
})

### fire.on.NA and fire.on.NULL = 'always'
test_that('Conditional reactive always triggers on NAs and NULLs', {
  testServer(conditionalServer(fire.on.NULL='always',fire.on.NA='always'), {
  	expect_equal(i(), 0)
  	expect_null(x())
    session$setInputs(x = NA)
    expect_true(is.na(y()))
    expect_equal(i(), 1, label='i() after setting "x = NA"')  ## the reactive 'y' is triggered by the initial NA/NULL

    session$setInputs(x = NULL)
  	expect_null(y())
    expect_equal(i(), 2, label='i() after setting "x = NULL"')

  	session$setInputs(x = NA)
  	expect_true(is.na(y()))
    expect_equal(i(), 3, label='i() after re-setting x back to "NA"')

  	session$setInputs(x = 1)
  	expect_equal(y(), 1)
    expect_equal(i(), 4)

  	session$setInputs(x = 1)
  	expect_equal(y(), 1)
    expect_equal(i(), 4)

  	session$setInputs(x = NA)
  	expect_true(is.na(y()))
    expect_equal(i(), 5, label='i() after setting "x = NA", after x was 1')

  	session$setInputs(x = NULL)
  	expect_null(y())
    expect_equal(i(), 6, label='i() after setting "x = NULL",  after x was 1')

  })
})

## Test reactives, when their value "changes" -------------------------

reactiveServer <- function() {
  return(function(id) {
    moduleServer(id, function(input, output, session) {

      ## re-fires every second, but doesn't change
  	  x <- reactive({
  	    invalidateLater(1000, session)
  		  return(0)
  	  })

  	  # counters that increments when a reactive fires
  	  cnt <- reactiveValues(r=0L, o=0L, er=0L, oe=0L)

  	  # test how reactive responds
      r <- reactive({
        cnt$r <- isolate(cnt$r) + 1
        x()
      })

      # test how observe responds
      observe({
        x()
        cnt$o <- isolate(cnt$o) + 1
      })

      # test how "eventReactive" responds
      er <- reactive({
        cnt$er <- isolate(cnt$er) + 1
        x()
      }) %>% bindEvent(x())

      # test how "observeEvent" responds
      observe({
        cnt$oe <- isolate(cnt$oe) + 1
        x()
      }) %>% bindEvent(x())

      ## Now make it conditional
      y <- conditionalReactive(x)
      cond <- reactiveValues(r=0L, o=0L, er=0L, oe=0L)
      cr <- reactive({
        cond$r <- isolate(cond$r) + 1
        y()
      })

      observe({
        y()
        cond$o <- isolate(cond$o) + 1
      })

      cer <- reactive({
        cond$er <- isolate(cond$er) + 1
        x()
      }) %>% bindEvent(y)

      observe({
        cond$oe <- isolate(cond$oe) + 1
        x()
      }) %>% bindEvent(y)

      ## Now make it conditional, the wrong way
      wrong <- reactiveValues(r=0L, o=0L, er=0L, oe=0L)
      wr <- reactive({
        wrong$r <- isolate(wrong$r) + 1
        conditionalReactive(x)()
      })

      observe({
        conditionalReactive(x)()
        wrong$o <- isolate(wrong$o) + 1
      })

      wer <- reactive({
        wrong$er <- isolate(wrong$er) + 1
        x()
      }) %>% bindEvent(conditionalReactive(x))

      observe({
        wrong$oe <- isolate(wrong$oe) + 1
        x()
      }) %>% bindEvent(conditionalReactive(x))
    })
  })
}

# helper function
unpack.reactiveValues <- function(cnt) {
  unlist(reactiveValuesToList(cnt)[c('r','o','er','oe')])
}

test_that('Reactive updates every time its upstream  is triggered, despite no change in value, but not the conditionalReactive when used correctly', {
  testServer(reactiveServer(), {
  	expect_equal(x(), 0)
    expect_equal(unpack.reactiveValues(cnt), c(r=0, o=0, er=0, oe=0))
    expect_equal(unpack.reactiveValues(cond), c(r=0, o=0, er=0, oe=0))
    expect_equal(unpack.reactiveValues(wrong), c(r=0, o=0, er=0, oe=0))
    expect_equal(r(), 0)
    expect_equal(er(), 0)
    expect_equal(cr(), 0)
    expect_equal(cer(), 0)
    expect_equal(wr(), 0)
    expect_equal(wer(), 0)
    # reactive and "eventReactive" fired because we requested a value?
    expect_equal(unpack.reactiveValues(cnt), c(r=1, o=0, er=1, oe=0))
    expect_equal(unpack.reactiveValues(cond), unpack.reactiveValues(cnt))
    expect_equal(unpack.reactiveValues(wrong), unpack.reactiveValues(cnt))

    session$elapse(1005)
    expect_equal(x(), 0)
    expect_equal(unpack.reactiveValues(cnt), c(r=1, o=1, er=1, oe=1))
    expect_equal(unpack.reactiveValues(cond), c(r=1, o=1, er=1, oe=1))
    expect_equal(unpack.reactiveValues(wrong), c(r=1, o=1, er=1, oe=1))
    expect_equal(r(), 0)
    expect_equal(er(), 0)
    expect_equal(cr(), 0)
    expect_equal(cer(), 0)
    expect_equal(wr(), 0)
    expect_equal(wer(), 0)
    # reactive and eventReactive re-fired because we requested a value?
    expect_equal(unpack.reactiveValues(cnt), c(r=2, o=1, er=2, oe=1))
    expect_equal(unpack.reactiveValues(cond), c(r=1, o=1, er=1, oe=1))
    expect_equal(unpack.reactiveValues(wrong), c(r=1, o=1, er=1, oe=1))



    session$elapse(2005) ## 2 more seconds elapsed!
    expect_equal(x(), 0)
    # observe fired twice.
    expect_equal(unpack.reactiveValues(cnt), c(r=2, o=3, er=2, oe=3))
    expect_equal(unpack.reactiveValues(cond), c(r=1, o=1, er=1, oe=1))
    expect_equal(unpack.reactiveValues(wrong), c(r=1, o=1, er=1, oe=1))
    expect_equal(r(), 0)
    expect_equal(er(), 0)
    expect_equal(cr(), 0)
    expect_equal(cer(), 0)
    expect_equal(wr(), 0)
    expect_equal(wer(), 0)
    # reactive and eventReactive re-fired because we requested a value
    expect_equal(unpack.reactiveValues(cnt), c(r=3, o=3, er=3, oe=3))
    expect_equal(unpack.reactiveValues(cond), c(r=1, o=1, er=1, oe=1))
    expect_equal(unpack.reactiveValues(wrong), c(r=1, o=1, er=1, oe=1))
  })
})


test_that('conditionalReactive can be used to stop unwanted behaviours - or combined in render-methods', {
  # but you really should be using `req` for that...
  testServer(function(id) {
    moduleServer(id, function(input, output, session) {
      x <- conditionalReactive(reactive(input$x), checkFun = function(x,y) {
        return(x < 10 && x != y)
      })

      output$res <- renderText({ stopifnot(x() < 10); x() })
      output$res2 <- renderText({
        message(isolate(input$y))
        conditionalReactive(reactive(input$x), checkFun = function(x,y) {
          return(x < 10 && x != y)
        })()
      })
    })
  }, expr = {
    expect_message(session$setInputs(x=5, y=1), '1')
    expect_equal(x(), 5)
    expect_equal(output$res, '5')
    expect_equal(output$res2, '5')


    session$setInputs(x=10) ## no messages!
    expect_equal(output$res, '5')

    expect_message(session$setInputs(x=6, y=2), '2')
    expect_equal(x(), 6)
    expect_equal(output$res, '6')
    expect_equal(output$res2, '6')
  })
})
