context("options")


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

test_that("Captured options contain expected elements", {
  shinyOptions(bookmarkStore = 123)
  shinyOptions(appDir = normalizePath("../")) # stomped
  caps <- captureAppOptions()
  
  expect_equal(sort(names(caps)), c("appDir", "bookmarkStore")
  expect_equal(caps$appDir, getwd())
  expect_equal(caps$bookmarkStore, 123)

  # verify that options are reset
  expect_equal(getShinyOption("bookmarkStore"), NULL)
  expect_equal(getShinyOption("appDir"), NULL)
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
