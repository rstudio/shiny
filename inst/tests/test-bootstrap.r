context("bootstrap")

test_that("CSS unit validation", {
  # On error, return NA; on success, return result
  validateCssUnit_wrap <- function(x) {
    tryCatch(validateCssUnit(x), error = function(e) { NA_character_ })
  }

  # Test strings and expected results
  strings  <- c("100x", "10px", "10.4px", ".4px", "1px0", "px", "5", "%", "5%", "auto", "1auto", "") 
  expected <- c(NA,     "10px", "10.4px", ".4px", NA,     NA,   NA,  NA,  "5%", "auto", NA,      NA)
  results <- vapply(strings, validateCssUnit_wrap, character(1), USE.NAMES = FALSE)
  expect_equal(results, expected)

  # Numbers should return string with "px"
  expect_equal(validateCssUnit(100), "100px")
})
