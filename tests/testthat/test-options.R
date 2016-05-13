context("options")

sortByName <- function(x) {
  if (anyUnnamed(x))
    stop("Can't sort by name because there are unnamed items")

  if (any(duplicated(names(x))))
    stop("Can't sort by name because there are duplicate names")

  x[sort(names(x))]
}

test_that("Option frames", {
  # Basic options
  setShinyOption(a = 1, b = 2)

  expect_identical(sortByName(shinyOptions()), sortByName(list(a = 1, b = 2)))
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)

  # Options that haven't been set
  expect_identical(getShinyOption('c'), NULL)
  expect_identical(getShinyOption('c', default = 10), 10)

  withNewOptionFrame({
    # No changes yet
    expect_identical(sortByName(shinyOptions()), sortByName(list(a = 1, b = 2)))
    expect_identical(getShinyOption('a'), 1)
    expect_identical(getShinyOption('b'), 2)

    # Override an option
    setShinyOption(a = 3)
    expect_identical(sortByName(shinyOptions()), sortByName(list(b = 2, a = 3)))
    expect_identical(getShinyOption('a'), 3)
    expect_identical(getShinyOption('b'), 2)

    # Options that haven't been set
    expect_identical(getShinyOption('c'), NULL)
    expect_identical(getShinyOption('c', default = 10), 10)

    # Another option frame
    withNewOptionFrame({
      # Override an option
      setShinyOption(a = 4)
      expect_identical(sortByName(shinyOptions()), sortByName(list(b = 2, a = 4)))
      expect_identical(getShinyOption('a'), 4)
      expect_identical(getShinyOption('b'), 2)
    })
  })

  # Should be back to original state
  expect_identical(shinyOptions(), list(a = 1, b = 2))
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)
})


# There's a difference between setting a value to NULL and un-setting it.
test_that("Un-setting options", {
  setShinyOption(a = 1, b = 2)
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)

  # Setting to a value and then setting to NULL should result in NULL.
  withNewOptionFrame({
    setShinyOption(a = 3)
    setShinyOption(a = NULL)
    expect_identical(getShinyOption('a'), NULL)
  })

  expect_identical(getShinyOption('a'), 1)

  # Setting to a value and then un-setting it should result in inherited value.
  withNewOptionFrame({
    setShinyOption(a = 3)
    unsetShinyOption('a')
    expect_identical(getShinyOption('a'), 1)
  })
})
