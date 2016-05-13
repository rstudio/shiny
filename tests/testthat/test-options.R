context("options")

sortByName <- function(x) {
  if (anyUnnamed(x))
    stop("Can't sort by name because there are unnamed items")

  if (any(duplicated(names(x))))
    stop("Can't sort by name because there are duplicate names")

  x[sort(names(x))]
}

test_that("Local options", {
  # Basic options
  shinyOptions(a = 1, b = 2)

  expect_identical(sortByName(shinyOptions()), sortByName(list(a = 1, b = 2)))
  expect_identical(getShinyOption('a'), 1)
  expect_identical(getShinyOption('b'), 2)

  # Options that haven't been set
  expect_identical(getShinyOption('c'), NULL)
  expect_identical(getShinyOption('c', default = 10), 10)

  withLocalOptions({
    # No changes yet
    expect_identical(sortByName(shinyOptions()), sortByName(list(a = 1, b = 2)))
    expect_identical(getShinyOption('a'), 1)
    expect_identical(getShinyOption('b'), 2)

    # Override an option
    shinyOptions(a = 3)
    expect_identical(sortByName(shinyOptions()), sortByName(list(b = 2, a = 3)))
    expect_identical(getShinyOption('a'), 3)
    expect_identical(getShinyOption('b'), 2)

    # Options that haven't been set
    expect_identical(getShinyOption('c'), NULL)
    expect_identical(getShinyOption('c', default = 10), 10)

    # Another local option set
    withLocalOptions({
      # Override an option
      shinyOptions(a = 4)
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
