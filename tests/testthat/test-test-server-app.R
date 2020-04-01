context("testServer app")

library(shiny)
library(testthat)

test_that("testServer works with dir app", {
  # app.R
  testServer(test_path("..", "test-modules", "06_tabsets"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })

  # server.R
  testServer(test_path("..", "test-modules", "server_r"), {
    session$setInputs(dist="norm", n=5)
    expect_length(d(), 5)

    session$setInputs(dist="unif", n=6)
    expect_length(d(), 6)
  })
})

test_that("testServer works when referencing external globals", {
  # If global is defined at the top of app.R outside of the server function.
  testServer(test_path("..", "test-modules", "06_tabsets"), {
    expect_equal(get("global", session$env), 123)
  })
})
