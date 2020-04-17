# Use testthat just for expectations
context("App")

testServer(expr = {
  # Set the `size` slider and check the output
  session$setInputs(size = 6)
  expect_equal(output$sequence, "1 2 3 4 5 6")

  session$setInputs(size = 12)
  expect_equal(output$sequence, paste0(lexical_sort(1:12), collapse = " "))
})
