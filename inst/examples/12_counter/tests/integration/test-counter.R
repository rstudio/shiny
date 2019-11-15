# Use testthat just for expectations
library(testthat)

testModule(counter, {
  session$setInputs(button=0)

  expect_equal(count(), 1)
  expect_equal(output$out, "1")

  # Simulate a click
  session$setInputs(button=1)

  expect_equal(count(), 2)
  expect_equal(output$out, "2")
})
