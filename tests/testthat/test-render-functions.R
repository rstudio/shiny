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
  expect_true(grepl("0\\.0", r1()))
  expect_true(grepl("20\\.0", r2()))
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
    "wrappedFunction"
  )
  # parents are not supported
  expect_label(
    (function(exprF) {quoToFunction(enquo0(exprF))})(),
    "wrappedFunction"
  )
})

for (info in list(
  list(
    name = "exprToFunction(expr, env, quoted)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      func <- exprToFunction(exprF, envF, quotedF)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    },
    can_not_test_quosures = TRUE
  ),
  list(
    name = "exprToFunction(expr, env, quoted = TRUE)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      if (!quotedF) exprF <- substitute(exprF)
      func <- exprToFunction(exprF, envF, quoted = TRUE)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    },
    can_not_test_quosures = TRUE
  ),
  list(
    name = "installExprFunction(expr, \"func\", env, quoted)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      installExprFunction(exprF, "func", envF, quotedF)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    },
    can_not_test_quosures = TRUE
  ),
  list(
    name = "installExprFunction(expr, \"func\", env, quoted = TRUE)",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      if (!quotedF) exprF <- substitute(exprF)
      installExprFunction(exprF, "func", envF, quoted = TRUE)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    },
    can_not_test_quosures = TRUE
  ),
  list(
    name = "sustainEnvAndQuoted(); quoToFunction()",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      q <- enquo0(exprF)
      q <- sustainEnvAndQuoted(q, exprF, envF, quotedF)
      func <- quoToFunction(q)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  ),
  list(
    name = "lower1 - sustainEnvAndQuoted(); quoToFunction()",
    fn = function(exprF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      q <- enquo0(exprF)
      q <- sustainEnvAndQuoted(q, exprF, envF, quotedF)
      function() {
        func <- quoToFunction(q)
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  ),
  list(
    name = "old args - sustainEnvAndQuoted(),quoToFunction()",
    fn = function(xF, envF = parent.frame(), quotedF = FALSE) {
      a <- 1000
      q <- enquo0(xF)
      # Eventually can remove this, once users stop using env and quoted
      q <- sustainEnvAndQuoted(q, xF, envF, quotedF)
      func <- quoToFunction(q)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  ),
  list(
    name = "deprecated args - sustainEnvAndQuoted(),quoToFunction()",
    fn = function(xF, envF = deprecated(), quotedF = deprecated()) {
      a <- 1000
      q <- enquo0(xF)
      # Eventually can remove this, once users stop using env and quoted
      q <- sustainEnvAndQuoted(q, xF, envF, quotedF)
      func <- quoToFunction(q)
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  ),
  list(
    name = "quoToFunction(enquo0(expr))",
    fn = function(expr) {
      func <- quoToFunction(enquo0(expr))
      function() {
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  ),
  list(
    name = "lower - quoToFunction(enquo0(expr))",
    fn = function(expr) {
      function() {
        func <- quoToFunction(enquo0(expr))
        value <- func()
        paste(rep(value, 2), collapse=", ")
      }
    }
  )
)) {

  # Scope the local variables
  local({
    renderH <- info$fn %||% stop("`info$fn` not found")
    messageVal <- info$messageVal %||% NA

    # Different usages of env and quoted param
    a <- 1
    e <- new.env()
    e$a <- 10

    test_that(paste0("vanilla: ", info$name), {
      expect_message({
        val <- renderH({a + 1})()
      }, messageVal)
      expect_identical(val, "2, 2")
    })


    if (length(formals(renderH)) > 1) {
      test_that(paste0("quoted = FALSE: ", info$name), {
        r <- renderH(a + 1, quotedF = FALSE)
        expect_identical(r(), "2, 2")
      })

      test_that(paste0("quoted = TRUE: ", info$name), {
        r <- renderH(quote(a + 1), quotedF = TRUE)
        expect_identical(r(), "2, 2")
      })

      test_that(paste0("env = e: ", info$name), {
        r <- renderH(a + 1, envF = e)
        expect_identical(r(), "11, 11")
      })

      test_that(paste0("env = e, quoted = FALSE: ", info$name), {
        r <- renderH(a + 1, envF = e, quotedF = FALSE)
        expect_identical(r(), "11, 11")
      })

      test_that(paste0("env = e, quoted = TRUE: ", info$name), {
        r <- renderH(quote(a + 1), envF = e, quotedF = TRUE)
        expect_identical(r(), "11, 11")
      })

      if (!isTRUE(info$can_not_test_quosures)) {
        test_that(paste0("Works with injecting raw quosures: ", info$name), {
          e <- list2env(list(a=10))
          x <- new_quosure(quote({ a + 1 }) , env = e)
          ans <- expect_message(
            inject(renderH(!!x))(),
            messageVal
          )
          expect_identical(ans, "11, 11")

          # All below should always error
          expect_error(inject(renderH(!!x, quotedF = F)), "alter your quosure")
          expect_error(inject(renderH(!!x, quotedF = T)), "alter your quosure")
          expect_error(inject(renderH(!!x, envF = e)), "alter your quosure")
          expect_error(inject(renderH(!!x, envF = environment())), "alter your quosure")
        })
      }
    }

    test_that(paste0("Works with inject / !!: ", info$name), {
      # Quosures
      a <- 1
      r1 <- inject(renderH({ !!a }))
      r2 <- renderH({ eval_tidy(quo(!!a)) })
      a <- 100
      expect_identical(r1(), "1, 1")
      expect_identical(r2(), "100, 100")
    })
  })
}


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
