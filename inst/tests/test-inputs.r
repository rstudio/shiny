context("inputs")

test_that("Date picker format translation", {

  # Default yyyy-mm-dd format
  str <- format(dateInput("date", "date label", value = "2012-02-29"))
  expect_match(str, 'data-date-format="yyyy-mm-dd"')
  expect_match(str, 'value="2012-02-29"')

  # mm/dd/yyyy format
  str <- format(dateInput("date", "date label", value = "02/29/12", format = "%m/%d/%y"))
  expect_match(str, 'data-date-format="mm/dd/yy"')
  expect_match(str, 'value="02/29/12"')

  # Pass in Date object
  str <- format(dateInput("date", "date label", value = as.Date("2012-02-29")))
  expect_match(str, 'data-date-format="yyyy-mm-dd"')
  expect_match(str, 'value="2012-02-29"')

  # Pass in Date object, expect different format for output
  str <- format(dateInput("date", "date label", value = as.Date("2012-02-29"), format = "%m/%d/%Y"))
  expect_match(str, 'data-date-format="mm/dd/yyyy"')
  expect_match(str, 'value="02/29/2012"')
})

