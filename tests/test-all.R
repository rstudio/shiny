
# only test if testthat is available
if (require(testthat)) {
  library(testthat)
  library(shiny)

  test_check("shiny")
}
