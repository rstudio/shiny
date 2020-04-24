library(testthat)

test_dir(
  "./testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
