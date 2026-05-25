test_that("Render functions correctly handle quosures", {
  # Normally, quosures are not unwrapped at creation time.
  # However, using inject() will make it unwrap at creation time.

  a <- 1
  r1 <- inject(renderText({ !!a }))
  r2 <- renderText({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1")
  expect_identical(r2(), "2")

  a <- 1
  r1 <- inject(renderPrint({ !!a }))
  r2 <- renderPrint({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "[1] 1")
  expect_identical(r2(), "[1] 2")

  a <- 1
  r1 <- inject(renderUI({ tags$p(!!a) }))
  r2 <- renderUI({ eval_tidy(quo(tags$p(!!a))) })
  a <- 2
  res1 <- r1(shinysession = MockShinySession$new(), name = "foo")
  expect_identical(as.character(res1$html), "<p>1</p>")
  res2 <- r2(shinysession = MockShinySession$new(), name = "foo")
  expect_identical(as.character(res2$html), "<p>2</p>")

  a <- 1
  r1 <- inject(renderTable({ pressure[!!a, ] }, digits = 1))
  r2 <- renderTable({ eval_tidy(quo(pressure[!!a, ])) }, digits = 1)
  a <- 2
  expect_match(r1(), "0\\.0")
  expect_match(r2(), "20\\.0")
})

test_that("functionLabel returns static value when the label can not be assigned to", {
  getFunc <- function(exprF, envF = parent.frame(), quotedF = FALSE) {
    quoToFunction(enquo0(exprF))
  }

  expect_label <- function(func, labely) {
    expect_equal(
      as.character(body(func)[[2]][[1]]),
      labely
    )
  }

  a <- 1

  expect_label(
    getFunc({a + 1}),
    "getFunc"
  )

  # multiline labels are not supported
  expect_label(
    (function(exprF) {
      quoToFunction(enquo0(exprF))
    })(),
    "anonymous"
  )
  # parents are not supported
  expect_label(
    (function(exprF) {quoToFunction(enquo0(exprF))})(),
    "anonymous"
  )
})

local({

# (must also copy logic into `lower - quoToFunction(enquo0(expr))` code)
return_func <- function(func) {
  function() {
    value <- func()
    list(value, value)
  }
}

for (info in list(
  list(
    name = "exprToFunction(expr, env, quoted)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      func <- exprToFunction(exprF, envF, quotedF)
      return_func(func)
    }
  ),
  list(
    name = "exprToFunction(expr, env, quoted = TRUE)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      # `exprF` coudl be a raw quosure if `inject()`ed
      if (!quotedF) exprF <- substitute(exprF)
      func <- exprToFunction(exprF, envF, quoted = TRUE)
      return_func(func)
    }
  ),
  list(
    name = "exprToFunction(expr, env, quoted = TRUE) + force()",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      # Make `exprF` always language, even if `inject()`ed
      if (!quotedF) exprF <- substitute(force(exprF))
      func <- exprToFunction(exprF, envF, quoted = TRUE)
      return_func(func)
    }
  ),
  list(
    name = "installExprFunction(expr, \"func\", env, quoted)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      installExprFunction(exprF, "func", envF, quotedF)
      return_func(func)
    }
  ),
  list(
    name = "installExprFunction(expr, \"func\", env, quoted = TRUE)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      # `exprF` coudl be a raw quosure if `inject()`ed
      if (!quotedF) exprF <- substitute(exprF)
      installExprFunction(exprF, "func", envF, quoted = TRUE)
      return_func(func)
    }
  ),
  list(
    name = "installExprFunction(expr, \"func\", env, quoted = TRUE)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      # Make `exprF` always language, even if `inject()`ed
      if (!quotedF) exprF <- substitute(force(exprF))
      installExprFunction(exprF, "func", envF, quoted = TRUE)
      return_func(func)
    }
  ),
  list(
    name = "quoToFunction(enquo0(expr))",
    fn = function(expr) {
      func <- quoToFunction(enquo0(expr))
      return_func(func)
    }
  ),
  list(
    name = "lower - quoToFunction(enquo0(expr))",
    fn = function(expr) {
      function() {
        func <- quoToFunction(enquo0(expr))
        value <- func()
        list(value, value)
      }
    }
  )
)) {

  # Scope the local variables
  local({
    renderH <- info$fn %||% stop("`info$fn` not found")

    # Different usages of env and quoted param
    a <- 1
    e <- new.env()
    e$a <- 10

    test_that(paste0("vanilla: ", info$name), {
      val <- renderH({a + 1})()
      expect_identical(val, list(2, 2))
    })

    # Test that no error is thrown when the function is created
    # This proves that the expression is not immediately evaluated
    test_that(paste0("stop('boom'): ", info$name), {
      expect_error(
        renderH(stop("boom")),
        NA
      )
    })

    if (length(formals(renderH)) > 1) {
      test_that(paste0("quoted = FALSE: ", info$name), {
        r <- renderH(a + 1, quotedF = FALSE)
        expect_identical(r(), list(2, 2))
      })

      test_that(paste0("quoted = TRUE: ", info$name), {
        r <- renderH(quote(a + 1), quotedF = TRUE)
        expect_identical(r(), list(2, 2))
      })

      test_that(paste0("env = e: ", info$name), {
        r <- renderH(a + 1, envF = e)
        expect_identical(r(), list(11, 11))
      })

      test_that(paste0("env = e, quoted = FALSE: ", info$name), {
        r <- renderH(a + 1, envF = e, quotedF = FALSE)
        expect_identical(r(), list(11, 11))
      })

      test_that(paste0("env = e, quoted = TRUE: ", info$name), {
        r <- renderH(quote(a + 1), envF = e, quotedF = TRUE)
        expect_identical(r(), list(11, 11))
      })

      test_that(paste0("Works with raw quosures, quoted = FALSE: ", info$name), {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)
        r <- renderH(x, quotedF = FALSE)
        expect_identical(r(), list(x, x))
      })
      test_that(paste0(
        "Passing in a raw quosures, quoted = FALSE, env = otherenv",
        " is treated like an expression: ", info$name),
      {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)
        other_env <- list2env(list(x=20))
        r <- renderH(x, quotedF = FALSE, envF = e)
        expect_identical(r(), list(x, x))
      })
      test_that(
        paste0("Works with injected quosures, quoted = FALSE, env = otherenv: ", info$name), {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)
        other_env <- new.env(parent = emptyenv())
        r <- inject(renderH(!!x, quotedF = FALSE, envF = e))
        expect_identical(r(), list(11, 11))
      })
      test_that(paste0("Works with raw quosures, quoted = TRUE: ", info$name), {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)
        ans <- renderH(x, quotedF = TRUE)()
        expect_identical(ans, list(11, 11))
      })
      test_that(paste0("Works with injecting raw quosures: ", info$name), {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)
        ans <- inject(renderH(!!x))()
        expect_identical(ans, list(11, 11))
      })

      test_that(paste0("Missing env with quosure, quoted = TRUE: ", info$name), {
        e <- list2env(list(a=10))
        x <- new_quosure(quote({ a + 1 }) , env = e)

        ans <- renderH(x, envF = rlang::missing_arg(), quotedF = TRUE)()
        expect_identical(ans, list(11, 11))
      })

    }

    test_that(paste0("Works with inject / !!: ", info$name), {
      # Quosures
      a <- 1
      r1 <- inject(renderH({ !!a }))
      r2 <- renderH({ eval_tidy(quo(!!a)) })
      a <- 100
      expect_identical(r1(), list(1,1))
      expect_identical(r2(), list(100, 100))
    })
  })
}
})



test_that("nested observe events work with exprToFunction", {

  val <- 0

  local({
    t1 <- reactiveVal(0)
    t2 <- reactiveVal(10)
    observeEvent(
      {
        # message("outer observeEvent trigger")
        val <<- val + 1
        t1()
      },
      {
        # message("outer observeEvent handler")
        val <<- val + 2
        observeEvent(
          {
            # message("inner observeEvent trigger")
            val <<- val + 3
            t2()
          },
          {
            val <<- val + 4
            # message("inner observeEvent handler")
          }
        )
      }
    )
  })

  expect_equal(val, 0)
  flushReact()
  expect_equal(val, 1 + 2 + 3 + 4)
})
