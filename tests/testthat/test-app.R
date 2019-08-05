
context("app")

test_that("helpers are loaded", {
  shiny:::loadHelpers("../test-helpers/app1-standard")
  expect_equal(helper1, 123)
  expect_equal(helper2, "abc")
})

test_that("nested helpers are loaded", {
  shiny:::loadHelpers("../test-helpers/app2-nested")
  expect_equal(helper1, 456)
  expect_equal(helper2, "def")
})

test_that("lower-case helper dir is loaded", {
  shiny:::loadHelpers("../test-helpers/app3-lowercase")
  expect_equal(helper1, 789)
})

test_that("app with both r/ and R/ prefers R/", {
  shiny:::loadHelpers("../test-helpers/app4-both")
  testthat::skip("Test not yet implemented")
})
