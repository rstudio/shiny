# See ?testServer for more information
testServer(exampleModuleServer, {
  # Set initial value of a button
  session$setInputs(button = 0)

  # Check the value of the reactiveVal `count()`
  expect_equal(count(), 1)
  # Check the value of the renderText()
  expect_equal(output$out, "1")

  # Simulate a click
  session$setInputs(button = 1)

  expect_equal(count(), 2)
  expect_equal(output$out, "2")
})
