context("testServer nesting")

library(shiny)
library(testthat)

test_that("Nested modules", {
  child <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$txt <- renderText("bar")
    })
  }

  parent <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$txt <- renderText("foo")
      child("child-id")
    })
  }

  testServer(parent, {
    expect_equal(output$txt, "foo")
  }, id = "parent-id")

})

test_that("Lack of ID", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$txt <- renderText(session$ns("x"))
    })
  }

  testServer(module, {
    expect_equal(output$txt, "foo-x")
  }, id = "foo")
})

test_that("testServer works with nested module servers", {
  outerModule <- function(id) {
    moduleServer(id, function(input, output, session) {
      r1 <- reactive({ input$x + 1})
      r2 <- innerModule("inner", r1)
      output$someVar <- renderText(r2())
    })
  }

  innerModule <- function(id, r) {
    moduleServer(id, function(input, output, session) {
      reactive(paste("a value:", r()))
    })
  }

  testServer(outerModule, {
    session$setInputs(x = 1)
    expect_equal(output$someVar, "a value: 2")
  }, id = "foo")
})

test_that("testServer calls do not nest in module functions", {
  module <- function(id) {
    moduleServer(id, function(input, output, session) {
      x <- 1
      testServer(function(id) {
        moduleServer(id, function(input, output, session) {
          y <- x + 1
        })
      })
    })
  }

  expect_error(testServer(module, {}), regexp = "Modules may not call testServer()")
})

test_that("testServer calls do not nest in test exprs", {
  module <- function(id) {
    x <- 1
    moduleServer(id, function(input, output, session) {
      inner <- function(id) {
        moduleServer(id, function(input, output, session) {
          y <- x + 1
        })
      }
    })
  }

  expect_error(testServer(module, {
    testServer(inner, {})
  }), regexp = "Test expressions may not call testServer()")
})
