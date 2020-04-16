
# We recommend using a testing framework.
# When testing, make sure to...
# 1. Execute your tests in your app's environment.
# 2. Throw an error (when appropriate) to tell `runTests()` that the test file failed.

expect_equal <- function(x, y) {
  if (!identical(x, y)) {
    stop("`", substitute(x), "` does not equal `", substitute(y), "`")
  }
}

withr::with_environment(
  # load app ./R files (and possibly global.R)
  shiny::loadSupport("../"),
  {

{{
# These blocks of code are processed with htmlTemplate()
if (isTRUE(module)) {
'
    # See ?testServer for more information
    testServer(mymoduleServer, expr = {
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
'
}
}}
{{
# These blocks of code are processed with htmlTemplate()
if (isTRUE(rdir)) {
'
    testServer("../", expr = {
      # TODO-barret remove path
      # Set the `size` slider and check the output
      session$setInputs(size = 6)
      expect_equal(output$sequence, "1 2 3 4 5 6")

      session$setInputs(size = 12)
      expect_equal(output$sequence, "1 10 11 12 2 3 4 5 6 7 8 9")
    })
'
}
}}
    if (!exists("lexical_sort")) {
      stop("lexical_sort was not loaded using loadSupport()")
    }

    if (!identical(
      lexical_sort(c(1, 2, 3, 13, 11, 21)),
      c(1, 11, 13, 2, 21, 3)
    )) {
      stop("lexical_sort did not lexically sort")
    }

  }
)
