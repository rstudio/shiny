context("options")

test_that("Local options", {
  # Basic options
  shinyOptions(a = 1, b = 2)

  expect_true(contents_identical(shinyOptions(),list(a = 1, b = 2)))
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)

  # Options that haven't been set
  expect_identical(getShinyOption('c'), NULL)
  expect_identical(getShinyOption('c', default = 10), 10)

  withLocalOptions({
    # No changes yet
    expect_true(contents_identical(shinyOptions(), list(a = 1, b = 2)))
    expect_identical(getShinyOption('a'), 1)
    expect_identical(getShinyOption('b'), 2)

    # Override an option
    shinyOptions(a = 3)
    expect_true(contents_identical(shinyOptions(), list(b = 2, a = 3)))
    expect_identical(getShinyOption('a'), 3)
    expect_identical(getShinyOption('b'), 2)

    # Options that haven't been set
    expect_identical(getShinyOption('c'), NULL)
    expect_identical(getShinyOption('c', default = 10), 10)

    # Another local option set
    withLocalOptions({
      # Override an option
      shinyOptions(a = 4)
      expect_true(contents_identical(shinyOptions(), list(b = 2, a = 4)))
      expect_identical(getShinyOption('a'), 4)
      expect_identical(getShinyOption('b'), 2)
    })
  })

  # Should be back to original state
  expect_true(contents_identical(shinyOptions(), list(a = 1, b = 2)))
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)

  # Setting options to NULL removes them entirely
  shinyOptions(b = NULL)
  expect_identical(shinyOptions(), list(a = 1))


  # Finish tests; reset shinyOptions
  shinyOptions(a = NULL)
})
