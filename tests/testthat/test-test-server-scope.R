context("testServer scope")

library(shiny)
library(testthat)

test_that("Variables outside of the module are inaccessible", {
  server <- local({
    outside <- 123
    function(id, x) {
      y <- x+1
      moduleServer(id, function(input, output, session) {
        z <- y+1
      })
    }
  }, envir = rlang::new_environment(parent = rlang::global_env()))

  testServer(server, {
    expect_equal(x, 0)
    expect_equal(y, 1)
    expect_equal(z, 2)
    expect_equal(exists("outside"), FALSE)
  }, x = 0)
})

test_that("Variables outside the testServer() have correct visibility", {
  server <- local({
    function(id, x) {
      moduleServer(id, function(input, output, session) {
        y <- 1
      })
    }
  }, envir = rlang::new_environment(parent = rlang::global_env()))

  x <- 99
  z <- 123

  testServer(server, {
    expect_equal(x, 0)
    expect_equal(y, 1)
    expect_equal(z, 123)
  }, x = 0)
})
