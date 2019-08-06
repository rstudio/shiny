
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
  ## App 4 already has a lower-case r/ directory. Try to create an upper.
  tryCatch(dir.create("../test-helpers/app4-both/R"),
           warning=function(w){testthat::skip("File system is not case-sensitive")})
  writeLines("upperHelper <- 'abc'", file.path("../test-helpers/app4-both/R", "upper.R"))

  shiny:::loadHelpers("../test-helpers/app4-both")

  expect_equal(lowerHelper, 123)
  expect_equal(upperHelper, "abc")
})
