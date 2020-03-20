
context("pkgdown")


test_that("pkgdown works", {
  skip_on_cran()

  # only test pkgdown from within `devtools::test()`
  ## `./tools` will not exist when shiny is installed
  pkgdown_file <- "../../tools/documentation/checkPkgdown.R"
  if (file.exists(pkgdown_file)) {
    source(pkgdown_file)
  }

  expect_true(TRUE)

})
