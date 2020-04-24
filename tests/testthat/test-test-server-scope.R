context("testServer scope")

library(shiny)
library(testthat)

test_that("Variables outside of the module are inaccessible", {
  module <- local({
    outside <- 123
    function(id, x) {
      y <- x+1
      moduleServer(id, function(input, output, session) {
        z <- y+1
      })
    }
  }, envir = new.env(parent = globalenv()))

  testServer(module, args = list(x = 0), {
    expect_equal(x, 0)
    expect_equal(y, 1)
    expect_equal(z, 2)
    expect_equal(exists("outside"), FALSE)
  })
})

test_that("Variables outside the testServer() have correct visibility", {
  module <- local({
    function(id, x) {
      moduleServer(id, function(input, output, session) {
        y <- 1
      })
    }
  }, envir = new.env(parent = globalenv()))

  x <- 99
  z <- 123

  testServer(module, args = list(x = 0), {
    expect_equal(x, 0)
    expect_equal(y, 1)
    expect_equal(z, 123)
  })
})

test_that("testServer allows lexical environment access through session$env", {
  module <- local({
    a_var <- 123
    function(id) {
      moduleServer(id, function(input, output, session) {
        b_var <- 321
      })
    }
  })

  expect_false(exists("a_var", inherits = FALSE))

  testServer(module, {
    expect_equal(b_var, 321)
    expect_equal(get("a_var", session$env, inherits = TRUE), 123)
    expect_false(exists("a_var", inherits = FALSE))
  })
})

test_that("Shadowing can be mitigated with unquote", {
  i <- 0
  inc <- function() i <<- i+1

  module <- local({
    function(id) {
      moduleServer(id, function(input, output, session) {
        inc <- function() stop("I should never be called")
      })
    }
  }, envir = globalenv())

  testServer(module, {
    expect_is(inc, "function")
    expect_false(identical(inc, !!inc))
    !!inc()
  })

  expect_equal(i, 1)
})
