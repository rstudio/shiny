# For issue #4281: `datesdisabled` silently did nothing when `format` was
# not the default `yyyy-mm-dd`, because it was rendered as a
# `data-date-*`-prefixed attribute, which bootstrap-datepicker picks up
# automatically and parses using the (possibly non-default) display format.
# `min`/`max` avoid this by using plain `data-min-date`/`data-max-date`
# attributes instead; `datesdisabled` should do the same.

test_that("dateInput() datesdisabled attribute is not consumed by bootstrap-datepicker's own data-date-* parsing", {
  input_attribs <- function(...) {
    tag <- dateInput(...)
    input_tag <- tag$children[[2]]
    input_tag$attribs
  }

  attribs <- input_attribs(
    "date8", "Date:",
    value = "2012-02-29",
    format = "dd/mm/yyyy",
    datesdisabled = c("2012-03-01", "2012-03-02")
  )

  # Must NOT use the `data-date-` prefix -- that's reserved for options that
  # bootstrap-datepicker parses itself using the display `format`.
  expect_null(attribs[["data-date-dates-disabled"]])

  # Instead, datesdisabled should be sent as plain yyyy-mm-dd strings (like
  # min/max), for the client-side binding to convert before handing off to
  # bootstrap-datepicker, regardless of the display `format`.
  expect_identical(
    unclass(attribs[["data-dates-disabled"]]),
    as.character(jsonlite::toJSON(c("2012-03-01", "2012-03-02")))
  )

  # Same JSON payload no matter what `format` is used for display.
  attribs_default_format <- input_attribs(
    "date8", "Date:",
    value = "2012-02-29",
    datesdisabled = c("2012-03-01", "2012-03-02")
  )
  expect_identical(
    attribs[["data-dates-disabled"]],
    attribs_default_format[["data-dates-disabled"]]
  )
})

test_that("dateInput() datesdisabled defaults to JSON null, not omitted", {
  tag <- dateInput("date8", "Date:")
  input_tag <- tag$children[[2]]

  expect_identical(unclass(input_tag$attribs[["data-dates-disabled"]]), "null")
})

test_that("dateInput() datesdisabled accepts Date objects", {
  tag <- dateInput(
    "date8", "Date:",
    format = "dd/mm/yyyy",
    datesdisabled = as.Date(c("2012-03-01", "2012-03-02"))
  )
  input_tag <- tag$children[[2]]

  expect_identical(
    unclass(input_tag$attribs[["data-dates-disabled"]]),
    as.character(jsonlite::toJSON(c("2012-03-01", "2012-03-02")))
  )
})
