
test_that("pkgdown works", {
  skip_on_cran()
  skip_if_not_installed("rprojroot")

  # These tests can only be run when we have access to the source
  # (i.e., they are skipped with `R CMD check`)
  pkgdown_file <- "../../tools/documentation/checkPkgdown.R"
  if (file.exists(pkgdown_file)) {
    source(pkgdown_file)
  } else {
    skip("Not able to verify whether the pkgdown reference is up-to-date")
  }
})
