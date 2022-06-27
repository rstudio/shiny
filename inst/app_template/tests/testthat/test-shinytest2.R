library(shinytest2)

test_that("Initial snapshot values are consistent", {
  app <- AppDriver$new(name = "init")
  app$expect_values()
}){{
if (isTRUE(module)) {
HTML('


test_that("Module values are consistent", {
  app <- AppDriver$new(name = "mod")
  app$click("examplemodule1-button")
  app$click("examplemodule1-button")
  app$expect_values()
})')
}
}}
