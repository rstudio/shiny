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


test_that("Custom render functions with correctly handle quosures", {
  # Four ways to create custom render functions:
  # - exprToFunction
  # - installExprFunction
  # - quoToFunction(expr, env, quoted)  <-- For backward compatbility
  # - quoToFunction(expr)               <-- Recommended way going forward

  # exprToFunction
  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    func <- shiny::exprToFunction(expr, env, quoted)
    function() {
      value <- func()
      paste(rep(value, 2), collapse=", ")
    }
  }
  a <- 1
  r1 <- inject(renderDouble({ !!a }))
  r2 <- renderDouble({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1, 1")
  expect_identical(r2(), "2, 2")


  # installExprFunction
  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    installExprFunction(expr, "func", env, quoted)
    function() {
      value <- func()
      paste(rep(value, 2), collapse=", ")
    }
  }
  a <- 1
  r1 <- inject(renderDouble({ !!a }))
  r2 <- renderDouble({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1, 1")
  expect_identical(r2(), "2, 2")


  # quoToFunction(expr, env, quoted)
  renderDouble <- function(expr, env = parent.frame(), quoted = FALSE) {
    q <- getQuosure(expr, env, quoted)
    func <- quoToFunction(q)
    function() {
      value <- func()
      paste(rep(value, 2), collapse=", ")
    }
  }
  a <- 1
  r1 <- inject(renderDouble({ !!a }))
  r2 <- renderDouble({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1, 1")
  expect_identical(r2(), "2, 2")
  # For this particular version, also make sure that it works with `env` and
  # `quoted`.
  e <- new.env()
  e$a <- 1
  r2 <- renderDouble(quote({ a }), env = e, quoted = TRUE)
  e$a <- 2
  expect_identical(r2(), "2, 2")


  # quoToFunction(expr)
  renderDouble <- function(expr) {
    q <- getQuosure(expr)
    func <- quoToFunction(q)
    function() {
      value <- func()
      paste(rep(value, 2), collapse=", ")
    }
  }
  a <- 1
  r1 <- inject(renderDouble({ !!a }))
  r2 <- renderDouble({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1, 1")
  expect_identical(r2(), "2, 2")})
