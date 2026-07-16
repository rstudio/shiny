test_that("an MCP restore query string builds an active input context", {
  rc <- RestoreContext$new("_inputs_&n=200&note=%22hi%22")
  expect_true(rc$active)
  expect_equal(rc$input$get("n"), 200)
  expect_equal(rc$input$get("note"), "hi")
})

test_that("createRestoreObservers is a method on ShinySession", {
  # Verify the extracted method exists as a public method on the ShinySession class
  expect_true("createRestoreObservers" %in% names(ShinySession$public_methods))
})
