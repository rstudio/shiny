# Wait for a promise (or plain value) and return its value or error message
resolve <- function(p) {
  if (!promises::is.promise(p)) {
    return(p)
  }
  done <- FALSE
  value <- NULL
  err <- NULL
  promises::then(
    p,
    function(x) {
      value <<- x
      done <<- TRUE
    },
    function(e) {
      err <<- e
      done <<- TRUE
    }
  )
  start <- Sys.time()
  while (!done) {
    later::run_now(0.01)
    if (difftime(Sys.time(), start, units = "secs") > 5) {
      stop("promise did not resolve")
    }
  }
  if (!is.null(err)) {
    stop(err)
  }
  value
}

# ---- hasAwait() ---------------------------------------------------------------

test_that("hasAwait() finds await() and await_each() calls", {
  expect_true(hasAwait(quote(await(p))))
  expect_true(hasAwait(quote(coro::await(p))))
  expect_true(hasAwait(quote(await_each(g))))
  expect_true(hasAwait(quote(coro::await_each(g))))
  expect_true(hasAwait(quote({
    x <- 1
    y <- await(p)
  })))
  expect_true(hasAwait(quote(if (a) b else await(p))))
  expect_true(hasAwait(quote(for (x in await_each(g)) total <- total + x)))
  expect_true(hasAwait(quote(f(g(h(await(p)))))))
  expect_true(hasAwait(quote(x[, await(p)])))
  expect_true(hasAwait(quote(isolate(await(p)))))
  expect_true(hasAwait(quote(tryCatch(await(p), error = function(e) NULL))))
})

test_that("hasAwait() ignores code without await()", {
  expect_false(hasAwait(NULL))
  expect_false(hasAwait(1))
  expect_false(hasAwait("await(p)"))
  expect_false(hasAwait(quote(await)))
  expect_false(hasAwait(quote(x)))
  expect_false(hasAwait(quote({
    x <- input$x
    x[, 1]
  })))
  expect_false(hasAwait(quote(other::await(p))))
  expect_true(hasAwait(quote(coro:::await(p))))
  # Bare names count only when listed; `coro::` always does
  expect_false(hasAwait(quote(await(p)), names = character()))
  expect_true(hasAwait(quote(coro::await(p)), names = character()))
  expect_false(hasAwait(quote(awaiting(p))))
  expect_false(hasAwait(quote(p %...>% print())))
})

test_that("hasAwait() skips await() inside nested scopes", {
  expect_false(hasAwait(quote(function() await(p))))
  expect_false(hasAwait(quote(\() await(p))))
  expect_false(hasAwait(quote({
    f <- function() await(p)
    1
  })))
  expect_false(hasAwait(quote(quote(await(p)))))
  expect_false(hasAwait(quote(async(function() await(p)))))
  expect_false(hasAwait(quote(coro::async(function() await(p)))))
  expect_false(hasAwait(quote(async_generator(function() await(p)))))
  expect_false(hasAwait(quote(coro::async_generator(function() await(p)))))
  expect_false(hasAwait(quote(generator(function() yield(1)))))
  expect_false(hasAwait(quote(lapply(xs, ~ await(.x)))))
  expect_false(hasAwait(quote(expr(await(p)))))
  expect_false(hasAwait(quote(rlang::expr(await(p)))))
  expect_false(hasAwait(quote(expression(await(p)))))
  # A nested scope does not hide an await() next to it
  expect_true(hasAwait(quote({
    f <- function() await(p)
    await(f())
  })))
})

# ---- Expression helpers -------------------------------------------------------

# `exprToFunction()` substitutes local variables when called with an unquoted
# expression from a function, so these tests pass quoted code.
asFunc <- function(expr, env = parent.frame()) {
  exprToFunction(expr, env, quoted = TRUE)
}

test_that("expression helpers leave code without await() alone", {
  f <- asFunc(quote({
    1 + 1
  }))
  expect_false(inherits(f, "coro_async"))
  expect_identical(f(), 2)
  expect_identical(fn_body(f), quote({
    1 + 1
  }))
  expect_identical(fnSourceBody(f), fn_body(f))
})

test_that("exprToFunction() turns await() code into a promise", {
  skip_if_not_installed("coro")
  y <- 10
  f <- asFunc(quote({
    x <- await(promises::promise_resolve(1))
    x + y
  }))
  p <- f()
  expect_true(promises::is.promise(p))
  expect_identical(resolve(p), 11)
  expect_identical(
    fnSourceBody(f),
    quote({
      x <- await(promises::promise_resolve(1))
      x + y
    })
  )
})

test_that("quoToFunction() and installExprFunction() support await()", {
  skip_if_not_installed("coro")
  f1 <- quoToFunction(rlang::quo({
    x <- await(promises::promise_resolve(3))
    x * 2
  }))
  expect_identical(resolve(f1()), 6)

  f2 <- installExprFunction(
    quote({
      x <- await(promises::promise_resolve(4))
      x * 2
    }),
    "func",
    environment(),
    quoted = TRUE,
    label = "test"
  )
  expect_identical(resolve(f2()), 8)
  expect_identical(resolve(func()), 8)
  expect_identical(
    installedFuncExpr(f2),
    quote({
      x <- await(promises::promise_resolve(4))
      x * 2
    })
  )
})

test_that("coro::await() and await_each() work", {
  skip_if_not_installed("coro")
  gen <- coro::async_generator(function() {
    for (i in 1:3) {
      coro::await(coro::async_sleep(0))
      coro::yield(i)
    }
  })
  f <- asFunc(quote({
    total <- 0
    for (x in coro::await_each(gen())) {
      total <- total + x
    }
    extra <- coro::await(promises::promise_resolve(100))
    total + extra
  }))
  expect_identical(resolve(f()), 106)
})

test_that("each call runs the async code again", {
  skip_if_not_installed("coro")
  n <- 0
  f <- asFunc(quote({
    await(coro::async_sleep(0))
    n <<- n + 1
    n
  }))
  expect_identical(resolve(f()), 1)
  expect_identical(resolve(f()), 2)
})

test_that("an error after await() rejects the promise", {
  skip_if_not_installed("coro")
  f <- asFunc(quote({
    await(coro::async_sleep(0))
    stop("boom")
  }))
  expect_error(resolve(f()), "boom")
})

test_that("a misplaced await() gets coro's error, not 'could not find'", {
  skip_if_not_installed("coro")
  # coro only allows `await()` as a statement or on the right of `<-`
  f <- asFunc(quote({
    paste("x", await(promises::promise_resolve(1)))
  }))
  expect_error(resolve(f()), "can't be called directly or within function")
})

test_that("a user's own await() function is left alone", {
  await <- function(x) paste("mine", x)
  f <- asFunc(quote({
    y <- await(1)
    y
  }))
  expect_false(inherits(f, "coro_async"))
  expect_identical(f(), "mine 1")
})

test_that("an attached coro's await() still counts", {
  skip_if_not_installed("coro")
  await <- coro::await
  f <- asFunc(quote({
    y <- await(promises::promise_resolve(1))
    y
  }))
  expect_s3_class(f, "coro_async")
  expect_identical(resolve(f()), 1)
})

test_that("the user's srcrefs are kept on the coro code", {
  skip_if_not_installed("coro")
  src <- "quo({\n  x <- await(promises::promise_resolve(1))\n  x\n})"
  q <- eval(parse(text = src, keep.source = TRUE))
  f <- quoToSimpleFunction(q)
  expect_s3_class(f, "coro_async")
  expect_false(is.null(attr(fnSourceBody(f), "srcref")))
})

test_that("assignments and `<<-` reach the user's environment", {
  skip_if_not_installed("coro")
  total <- 0
  f <- asFunc(quote({
    x <- await(promises::promise_resolve(5))
    total <<- total + x
    total
  }))
  expect_identical(resolve(f()), 5)
  expect_identical(total, 5)
  expect_false(exists("x", inherits = FALSE))
})

test_that("a missing coro gives a clear error", {
  local_mocked_bindings(
    check_installed = function(pkg, reason, ...) {
      stop("coro is required ", reason)
    },
    .package = "rlang"
  )
  expect_error(
    asFunc(quote(await(p))),
    "coro is required to use `await\\(\\)` in reactive code"
  )
  # Code without await() never asks for coro
  expect_identical(asFunc(1)(), 1)
})

# ---- reactive() ---------------------------------------------------------------

test_that("reactive() with await() tracks inputs read before and after await()", {
  skip_if_not_installed("coro")
  runs <- 0
  server <- function(input, output, session) {
    r <- reactive({
      runs <<- runs + 1
      x <- input$x
      y <- await(promises::promise_resolve(x + 1))
      await(coro::async_sleep(0))
      y * input$m
    })
  }
  testServer(server, {
    session$setInputs(x = 1, m = 2)
    expect_true(promises::is.promise(r()))
    expect_identical(resolve(r()), 4)
    expect_identical(runs, 1)

    # Cached until an input changes
    expect_identical(resolve(r()), 4)
    expect_identical(runs, 1)

    # Read before await()
    session$setInputs(x = 2)
    expect_identical(resolve(r()), 6)
    expect_identical(runs, 2)

    # Read after await()
    session$setInputs(m = 10)
    expect_identical(resolve(r()), 30)
    expect_identical(runs, 3)
  })
})

test_that("reactive() without await() still returns plain values", {
  server <- function(input, output, session) {
    r <- reactive(input$x * 10)
  }
  testServer(server, {
    session$setInputs(x = 2)
    expect_identical(r(), 20)
  })
})

test_that("a reactive with await() can be awaited by another one", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    a <- reactive({
      await(coro::async_sleep(0))
      input$x + 1
    })
    b <- reactive({
      x <- await(a())
      x * 2
    })
    output$txt <- renderText(b())
  }
  testServer(server, {
    session$setInputs(x = 1)
    expect_identical(resolve(b()), 4)
    expect_identical(output$txt, "4")
    session$setInputs(x = 4)
    expect_identical(resolve(b()), 10)
    expect_identical(output$txt, "10")
  })
})

test_that("reactive() with await() keeps the session domain after await()", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    r <- reactive({
      await(coro::async_sleep(0))
      getDefaultReactiveDomain()
    })
  }
  testServer(server, {
    session$setInputs(x = 1)
    expect_identical(resolve(r()), session)
  })
})

test_that("isolate() after await() does not take a dependency", {
  skip_if_not_installed("coro")
  runs <- 0
  server <- function(input, output, session) {
    r <- reactive({
      runs <<- runs + 1
      await(coro::async_sleep(0))
      input$x + isolate(input$y)
    })
  }
  testServer(server, {
    session$setInputs(x = 1, y = 1)
    expect_identical(resolve(r()), 2)
    session$setInputs(y = 5)
    expect_identical(resolve(r()), 2)
    expect_identical(runs, 1)
    session$setInputs(x = 2)
    expect_identical(resolve(r()), 7)
    expect_identical(runs, 2)
  })
})

test_that("reactive() labels and format show the user's code", {
  skip_if_not_installed("coro")
  r <- reactive({
    await(promises::promise_resolve(1))
  })
  expect_equal(as.character(attr(r, "observable")$.label), "r")

  expect_match(
    format(attr(reactive(await(promises::promise_resolve(1))), "observable")),
    "reactive({\n    await(promises::promise_resolve(1))\n})",
    fixed = TRUE
  )
  expect_identical(
    attr(r, "cacheHint")$userExpr,
    quote({
      await(promises::promise_resolve(1))
    })
  )
})

test_that("reactive() with await() and bindCache() caches by key", {
  skip_if_not_installed("coro")
  runs <- 0
  server <- function(input, output, session) {
    r <- reactive({
      runs <<- runs + 1
      await(coro::async_sleep(0))
      input$x * 2
    }) |>
      bindCache(input$x, cache = cachem::cache_mem())
  }
  testServer(server, {
    session$setInputs(x = 1)
    expect_identical(resolve(r()), 2)
    session$setInputs(x = 2)
    expect_identical(resolve(r()), 4)
    session$setInputs(x = 1)
    expect_identical(resolve(r()), 2)
    expect_identical(runs, 2)
  })
})

test_that("reactive() with await() and bindEvent() runs on the event", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    r <- reactive({
      await(coro::async_sleep(0))
      input$x
    }) |>
      bindEvent(input$go)
  }
  testServer(server, {
    session$setInputs(x = 1, go = 1)
    expect_identical(resolve(r()), 1)
    session$setInputs(x = 2)
    expect_identical(resolve(r()), 1)
    session$setInputs(go = 2)
    expect_identical(resolve(r()), 2)
  })
})

test_that("eventReactive() default label shows the user's event code", {
  skip_if_not_installed("coro")
  r <- isolate(eventReactive({ x <- await(promises::promise_resolve(1)); x }, 1))
  label <- as.character(attr(r, "observable")$.label)
  expect_match(label, "x <- await(promises::promise_resolve(1))", fixed = TRUE)
  expect_no_match(label, "_private", fixed = TRUE)
})

test_that("eventReactive() and observeEvent() event code supports await()", {
  skip_if_not_installed("coro")
  seen <- c()
  server <- function(input, output, session) {
    r <- eventReactive({
      v <- await(promises::promise_resolve(input$go))
      v
    }, input$go * 10)
    observeEvent({
      v <- await(promises::promise_resolve(input$go))
      v
    }, {
      seen <<- c(seen, input$go)
    })
  }
  testServer(server, {
    # ignoreNULL looks at the resolved value
    session$setInputs(x = 1)
    expect_identical(seen, c())
    session$setInputs(go = 1)
    expect_identical(resolve(r()), 10)
    expect_identical(seen, 1)
  })
})

test_that("await() in one of several keys gives a clear error", {
  skip_if_not_installed("coro")
  r <- reactive(1)
  expect_error(
    bindEvent(r, 1, { x <- await(p); x }),
    "single cache key or event expression"
  )
})

test_that("eventReactive() value code supports await()", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    r <- eventReactive(input$go, {
      await(coro::async_sleep(0))
      input$x * 3
    })
  }
  testServer(server, {
    session$setInputs(x = 2, go = 1)
    expect_identical(resolve(r()), 6)
    session$setInputs(x = 3)
    expect_identical(resolve(r()), 6)
    session$setInputs(go = 2)
    expect_identical(resolve(r()), 9)
  })
})

# ---- observe() ----------------------------------------------------------------

test_that("observe() with await() runs and tracks inputs read after await()", {
  skip_if_not_installed("coro")
  seen <- c()
  server <- function(input, output, session) {
    observe({
      x <- input$x
      await(promises::promise_resolve(NULL))
      seen <<- c(seen, x, input$y)
    })
  }
  testServer(server, {
    session$setInputs(x = 1, y = 10)
    expect_identical(seen, c(1, 10))
    session$setInputs(x = 2)
    expect_identical(seen, c(1, 10, 2, 10))
    # input$y is read only after await()
    session$setInputs(y = 20)
    expect_identical(seen, c(1, 10, 2, 10, 2, 20))
  })
})

test_that("observeEvent() handler supports await()", {
  skip_if_not_installed("coro")
  seen <- c()
  server <- function(input, output, session) {
    observeEvent(input$go, {
      v <- await(promises::promise_resolve(input$x))
      seen <<- c(seen, v)
    })
  }
  testServer(server, {
    session$setInputs(x = "a", go = 1)
    session$setInputs(x = "b")
    session$setInputs(go = 2)
    expect_identical(seen, c("a", "b"))
  })
})

# ---- render*() ----------------------------------------------------------------

test_that("render functions support await()", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    output$txt <- renderText({
      await(coro::async_sleep(0))
      paste("x is", input$x)
    })
    output$prt <- renderPrint({
      await(promises::promise_resolve(input$x * 2))
    })
    output$ui <- renderUI({
      x <- await(promises::promise_resolve(input$x))
      tags$b(x)
    })
    output$tbl <- renderTable({
      await(coro::async_sleep(0))
      data.frame(x = input$x)
    })
  }
  testServer(server, {
    session$setInputs(x = 1)
    expect_identical(output$txt, "x is 1")
    expect_identical(output$prt, "[1] 2")
    expect_match(output$ui$html, "<b>1</b>", fixed = TRUE)
    expect_match(output$tbl, "<td", fixed = TRUE)

    session$setInputs(x = 5)
    expect_identical(output$txt, "x is 5")
    expect_identical(output$prt, "[1] 10")
    expect_match(output$ui$html, "<b>5</b>", fixed = TRUE)
  })
})

test_that("renderPlot() supports await()", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    output$plt <- renderPlot({
      n <- await(promises::promise_resolve(input$n))
      plot(seq_len(n))
    })
  }
  testServer(server, {
    session$setInputs(n = 3)
    expect_match(output$plt$src, "^data:image/png;base64,")
  })
})

test_that("req() after await() leaves the output silent", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    output$txt <- renderText({
      await(coro::async_sleep(0))
      req(input$x)
      paste("x is", input$x)
    })
  }
  testServer(server, {
    session$setInputs(x = NULL)
    expect_error(output$txt, class = "shiny.silent.error")
    session$setInputs(x = 1)
    expect_identical(output$txt, "x is 1")
  })
})

test_that("an error after await() shows in the output", {
  skip_if_not_installed("coro")
  server <- function(input, output, session) {
    output$txt <- renderText({
      await(coro::async_sleep(0))
      validate(need(input$x > 0, "x must be positive"))
      input$x
    })
  }
  testServer(server, {
    session$setInputs(x = -1)
    expect_error(output$txt, "x must be positive")
    session$setInputs(x = 2)
    expect_identical(output$txt, "2")
  })
})

test_that("render functions with await() get distinct cache hints", {
  skip_if_not_installed("coro")
  a <- renderText({
    await(promises::promise_resolve("a"))
  })
  b <- renderText({
    await(promises::promise_resolve("b"))
  })
  expect_false(identical(attr(a, "cacheHint"), attr(b, "cacheHint")))
  expect_identical(
    attr(a, "cacheHint")$origUserFunc$body,
    quote({
      await(promises::promise_resolve("a"))
    })
  )
})

test_that("render functions with await() and bindCache() cache by key", {
  skip_if_not_installed("coro")
  runs <- 0
  server <- function(input, output, session) {
    output$txt <- renderText({
      runs <<- runs + 1
      await(coro::async_sleep(0))
      input$x * 2
    }) |>
      bindCache(input$x, cache = cachem::cache_mem())
  }
  testServer(server, {
    session$setInputs(x = 1)
    expect_identical(output$txt, "2")
    session$setInputs(x = 2)
    expect_identical(output$txt, "4")
    session$setInputs(x = 1)
    expect_identical(output$txt, "2")
    expect_identical(runs, 2)
  })
})

# ---- Stack traces -------------------------------------------------------------

# Run reactive code from `lines` (as file "app.R") and return the printed error
# header and stack trace
printedAsyncError <- function(lines) {
  # coro throws an error before the first `await()` right away
  err <- tryCatch(
    resolve(eval(parse(
      text = lines,
      keep.source = TRUE,
      srcfile = srcfilecopy("app.R", lines)
    ))),
    error = identity
  )
  header <- NULL
  trace <- capture.output(
    withCallingHandlers(
      printError(err),
      warning = function(w) {
        header <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    type = "message"
  )
  list(header = header, trace = trace)
}

test_that("a stack trace after await() shows the reactive and the user's line", {
  skip_if_not_installed("coro")
  out <- printedAsyncError(c(
    "captureStackTraces(isolate(reactive(label = 'r', {",
    "  x <- await(promises::promise_resolve(1))",
    "  stop('boom')",
    "})()))"
  ))
  expect_identical(out$header, "Error in <reactive:r>: boom")
  expect_match(out$trace, "<reactive:r> [app.R#3]", fixed = TRUE, all = FALSE)
  # coro's frames tell the user nothing
  expect_no_match(out$trace, "eval_bare|evalq|wrapOnFulfilled")
})

test_that("a stack trace before await() hides coro's frames", {
  skip_if_not_installed("coro")
  out <- printedAsyncError(c(
    "captureStackTraces(isolate(reactive(label = 'r', {",
    "  stop('boom')",
    "  x <- await(promises::promise_resolve(1))",
    "})()))"
  ))
  expect_identical(out$header, "Error in <reactive:r>: boom")
  expect_match(out$trace, "<reactive:r> [app.R#2]", fixed = TRUE, all = FALSE)
  expect_no_match(out$trace, "eval_bare|evalq|instance")
})
