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
  old_options <- shinyOptions(); on.exit(do.call(shinyOptions, old_options))

  shinyOptions(bookmarkStore = 123)
  shinyOptions(appDir = normalizePath("../")) # stomped
  caps <- captureAppOptions()

  expect_equal(sort(names(caps)), c("appDir", "bookmarkStore"))
  expect_equal(caps$appDir, getwd())
  expect_equal(caps$bookmarkStore, 123)

  # verify that options are reset
  expect_equal(getShinyOption("bookmarkStore"), NULL)
  expect_equal(getShinyOption("appDir"), NULL)
})

test_that("Capturing options at creation time", {
  old_options <- shinyOptions(); on.exit(do.call(shinyOptions, old_options))

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
  runApp(s2, launch.browser = FALSE, quiet = TRUE)
  expect_identical(value, NULL)
  expect_identical(value2, 456)
  expect_identical(getShinyOption("bookmarkStore"), NULL)

  # Same, for first app and value1.
  runApp(s, launch.browser = FALSE, quiet = TRUE)
  expect_identical(value, 123)
  expect_identical(value2, 456)
  expect_identical(getShinyOption("bookmarkStore"), NULL)
})


test_that("Option scoping", {
  old_options <- shinyOptions(); on.exit(do.call(shinyOptions, old_options))
  # Tests for global and app-level scoping for shinyOptions().
  shinyOptions(foo = NULL)
  values <- list()
  values["global_in"] <- list(getShinyOption("foo"))
  shinyOptions(foo = "global")

  s <- shinyApp(
    basicPage(),
    function(input, output, session) {},
    onStart = function() {
      values["app_in"] <<- list(getShinyOption("foo"))

      shinyOptions(foo = "app")

      onStop(function() {
        values["app_out"] <<- list(getShinyOption("foo"))
      })
      observe(stopApp())
    }
  )

  # Run the app
  runApp(s, launch.browser = FALSE, quiet = TRUE)
  values["global_out"] <- list(getShinyOption("foo"))

  expect_identical(
    values,
    list(
      global_in = NULL,
      app_in = "global",
      app_out = "app",
      global_out = "global"
    )
  )

  # Note: The code below also tests for session-level scoping of shinyOptions(),
  # but it can't be easily run in CI, since it requires a browser to connect to
  # the app to run the session-level code.
  #
  # # Tests for global, app, and session-level scoping for shinyOptions().
  # shinyOptions(foo = NULL)
  # values <- list()
  # values["global_in"] <- list(getShinyOption("foo"))
  # shinyOptions(foo = "global")
  #
  # s <- shinyApp(
  #   basicPage(),
  #   function(input, output, session) {
  #     values["session_in"] <<- list(getShinyOption("foo"))
  #     shinyOptions(foo = "session")
  #     onStop(function() {
  #       values["session_out"] <<- list(getShinyOption("foo"))
  #     })
  #
  #     observe(stopApp())
  #   },
  #   onStart = function() {
  #     values["app_in"] <<- list(getShinyOption("foo"))
  #
  #     shinyOptions(foo = "app")
  #
  #     onStop(function() {
  #       values["app_out"] <<- list(getShinyOption("foo"))
  #     })
  #   }
  # )
  #
  # # Run the app
  # runApp(s)
  # # Need this to run the session's onStop callbacks. But this should happen
  # # automatically, without needing to pump the event loop an extra time.
  # later::run_now()
  # values["global_out"] <- list(getShinyOption("foo"))
  #
  # expect_identical(
  #   values,
  #   list(
  #     global_in = NULL,
  #     app_in = "global",
  #     session_in = "app",
  #     app_out = "app",
  #     # TODO: It would be more correct if session_out came before app_out,
  #     # because it makes more sense for the session's onStop callback to run
  #     # before the app's onStop. But that can be revisited at some point in the
  #     # future.
  #     session_out = "session",
  #     global_out = "global"
  #   )
  # )

})

test_that("Unsetting app-level option NULL won't affect global option", {
  old_options <- shinyOptions(); on.exit(do.call(shinyOptions, old_options))

  shinyOptions(foo = 1)
  value <- NULL
  s <- shinyApp(basicPage(), function(input, output) {},
    onStart = function() {
      shinyOptions(foo = NULL)
      value <<- getShinyOption("foo")
      observe(stopApp())
    }
  )

  runApp(s, launch.browser = FALSE, quiet = TRUE)
  expect_identical(getShinyOption("foo"), 1)
  expect_identical(value, NULL)
})
