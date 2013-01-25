context("invisibility")

test_that("invisibility is preserved", {
  funcA <- function() invisible()
  funcB <- function() NULL
  funcC <- reactive(funcA)
  funcD <- reactive(funcB)
  
  expect_equal(withVisible(isolate(funcA()))$visible, FALSE)
  expect_equal(withVisible(isolate(funcB()))$visible, TRUE)
  expect_equal(withVisible(isolate(funcC()))$visible, FALSE)
  expect_equal(withVisible(isolate(funcD()))$visible, TRUE)
})
