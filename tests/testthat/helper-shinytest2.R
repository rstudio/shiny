# Call at the top of any shinytest2 test file to skip in environments where
# browser-based tests cannot run.
skip_if_not_shinytest2 <- function() {
  skip_on_cran()
  skip_on_os(c("windows", "linux", "solaris", "emscripten"))
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
  app <- shinytest2::AppDriver$new(app_fn, ...)
  withr::defer(app$stop(), envir = envir)
  app$wait_for_idle()
  app
}
