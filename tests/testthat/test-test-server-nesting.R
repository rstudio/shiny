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
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$txt <- renderText(session$ns("x"))
    })
  }

  testServer(server, {
    expect_equal(output$txt, "foo-x")
  }, id = "foo")
})

test_that("testServer works with nested module servers", {
  outerServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      r1 <- reactive({ input$x + 1})
      r2 <- innerServer("inner", r1)
      output$someVar <- renderText(r2())
    })
  }

  innerServer <- function(id, r) {
    moduleServer(id, function(input, output, session) {
      reactive(paste("a value:", r()))
    })
  }

  testServer(outerServer, {
    session$setInputs(x = 1)
    expect_equal(output$someVar, "a value: 2")
  }, id = "foo")
})

test_that("testServer calls do not nest in module functions", {
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      x <- 1
      testServer(function(id) {
        moduleServer(id, function(input, output, session) {
          y <- x + 1
        })
      })
    })
  }

  expect_error(testServer(server, {}), regexp = "Modules may not call testServer()")
})

test_that("testServer calls do not nest in test exprs", {
  server <- function(id) {
    x <- 1
    moduleServer(id, function(input, output, session) {
      inner <- function(id) {
        moduleServer(id, function(input, output, session) {
          y <- x + 1
        })
      }
    })
  }

  expect_error(testServer(server, {
    testServer(inner, {})
  }), regexp = "Test expressions may not call testServer()")
})
