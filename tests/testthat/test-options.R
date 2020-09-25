context("options")

test_that("Local options", {
  # Clear out any options so we know we're starting fresh
  .globals$options <- list()

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

test_that("Shared secret", {
  op <- options(shiny.sharedSecret = "This is a secret string")
  on.exit(options(op))

  checkSharedSecret <- loadSharedSecret()

  expect_true(checkSharedSecret("This is a secret string"))
  expect_true(checkSharedSecret(charToRaw("This is a secret string")))

  expect_false(checkSharedSecret("this is a secret string"))
  expect_false(checkSharedSecret("This is a secret string "))
  expect_false(checkSharedSecret(""))
  expect_false(checkSharedSecret(NULL))
  expect_error(checkSharedSecret(1:10))
})


test_that("Capturing options at creation time", {
  # These are tests of captureAppOptions. Specific options should be captured
  # and stored at app creation time and immediately reset. If another app is
  # created, it will also capture these options at creation time.
  # When these apps are started, they will apply the options.
  shinyOptions(bookmarkStore = 123)
  value <- NULL
  s <- shinyApp(basicPage(), function(input, output) {},
    onStart = function() {
      value <<- getShinyOption("bookmarkStore")
      observe(stopApp())
    }
  )
  # After creating app, bookmarkStore option should be reset
  expect_identical(getShinyOption("bookmarkStore"), NULL)
  expect_identical(value, NULL)

  # Create another app
  shinyOptions(bookmarkStore = 456)
  value2 <- NULL
  s2 <- shinyApp(basicPage(), function(input, output) {},
    onStart = function() {
      value2 <<- getShinyOption("bookmarkStore")
      observe(stopApp())
    }
  )
  expect_identical(getShinyOption("bookmarkStore"), NULL)
  expect_identical(value2, NULL)

  # Running second app will cause the option to be restored while it's running,
  # and in our onStart, we save it into value2.
  runApp(s2, launch.browser = FALSE)
  expect_identical(value, NULL)
  expect_identical(value2, 456)
  expect_identical(getShinyOption("bookmarkStore"), NULL)

  # Same, for first app and value1.
  runApp(s, launch.browser = FALSE)
  expect_identical(value, 123)
  expect_identical(value2, 456)
  expect_identical(getShinyOption("bookmarkStore"), NULL)
})
