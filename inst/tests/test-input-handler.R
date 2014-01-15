context("Parse Shiny Input")

test_that("A new type can be registered successfully", {
  registerInputHandler("shiny.someType", function(){})
})

test_that("A duplicated type throws", {
  expect_error({
    registerInputHandler("shiny.dupType", function(){})
    registerInputHandler("shiny.dupType", function(){})
  })
})
