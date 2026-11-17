# Call at the top of any shinytest2 test file to skip in environments where
# browser-based tests cannot run.
skip_if_not_shinytest2 <- function() {
  skip_on_cran()
  skip_on_os(c("windows", "solaris", "emscripten"))
  skip_if_not_installed("shinytest2")
}

# Launch an inline shinytest2 app and register cleanup in the caller's frame.
# Returns the AppDriver instance ready for use.
#
# Usage:
#   app <- launch_test_app(function() {
#     library(shiny)
#     shinyApp(ui, server)
#   })
launch_test_app <- function(app_fn, ..., envir = parent.frame()) {
  # Under devtools::test(), `app_fn`'s environment inherits from the shiny
  # namespace. callr serializes that link, so the app subprocess loads the
  # *installed* shiny to rebuild `app_fn`, and the app runs that version
  # instead of the working tree. Pointing `app_fn` at the global environment
  # drops the link, so `library(shiny)` inside `app_fn` loads the working tree.
  # `app_fn` must not use variables from the test file.
  environment(app_fn) <- globalenv()
  app <- shinytest2::AppDriver$new(app_fn, ...)
  withr::defer(app$stop(), envir = envir)
  app$wait_for_idle()
  app
}
