# Call at the top of any shinytest2 test file to skip in environments where
# browser-based tests cannot run.
skip_if_not_shinytest2 <- function() {
  skip_on_cran()
  skip_on_os(c("windows", "linux", "solaris", "emscripten"))
  skip_if_not_installed("callr")
  skip_if_not_installed("httr")
  skip_if_not_installed("shinytest2")
}
