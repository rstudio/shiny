test_that("check_required() reports the calling function and missing args", {
  f <- function(a, b, c = NULL) check_required(a, b)

  # No missing args: returns invisibly without error
  expect_silent(f(1, 2))

  # One missing arg: singular
  expect_error(f(1), "`f\\(\\)` is missing required argument: `b`")

  # Multiple missing args: pluralized
  expect_error(f(), "`f\\(\\)` is missing required arguments: `a` and `b`")

  # Explicit NULL counts as supplied
  expect_silent(f(NULL, NULL))

  # Explicit NA counts as supplied
  expect_silent(f(NA, NA))
})

test_that("missing-arg errors carry the shiny_missing_arg_error class", {
  expect_error(textInput("id"),    class = "shiny_missing_arg_error")
  expect_error(sliderInput("id"),  class = "shiny_missing_arg_error")
  expect_error(actionButton("id"), class = "shiny_missing_arg_error")
})

test_that("check_required() works under do.call()", {
  # do.call() loses the call name, but the missing argument(s) are still
  # reported correctly.
  expect_error(
    do.call(textInput, list(inputId = "id")),
    "is missing required argument: `label`"
  )
})

test_that("missing required args error before downstream R errors", {
  # Before the fix these produced messages like
  #   `argument "label" is missing, with no default`
  # which named neither the function nor the argument. Confirm the new
  # message includes both.
  expect_error(textInput("id"),         "`textInput\\(\\)`.*`label`")
  expect_error(sliderInput("id", "L"),  "`sliderInput\\(\\)`.*`min`")
  expect_error(selectInput("id", "L"),  "`selectInput\\(\\)`.*`choices`")
  expect_error(actionButton("id"),      "`actionButton\\(\\)`.*`label`")
})

test_that("explicit NULL/NA satisfies required args (documented opt-out)", {
  # `label = NULL` is documented as "no label" — must not be rejected.
  expect_silent(textInput("id", NULL))
  expect_silent(checkboxInput("id", NULL))
  expect_silent(actionButton("id", NULL))
})

test_that("happy paths still construct a shiny.tag without error", {
  expect_tag <- function(x) {
    expect_s3_class(x, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
  }
  expect_tag(textInput("id", "Label"))
  expect_tag(textAreaInput("id", "Label"))
  expect_tag(passwordInput("id", "Label"))
  expect_tag(numericInput("id", "Label", value = 1))
  expect_tag(checkboxInput("id", "Label"))
  expect_tag(checkboxGroupInput("id", "Label", choices = "a"))
  expect_tag(radioButtons("id", "Label", choices = "a"))
  expect_tag(selectInput("id", "Label", choices = "a"))
  expect_tag(varSelectInput("id", "Label", data = mtcars))
  expect_tag(dateInput("id", "Label"))
  expect_tag(dateRangeInput("id", "Label"))
  expect_tag(fileInput("id", "Label"))
  expect_tag(sliderInput("id", "Label", min = 1, max = 10, value = 5))
  expect_tag(actionButton("id", "Label"))
  expect_tag(actionLink("id", "Label"))
})

test_that("missing required input args produce informative errors", {
  expect_snapshot(error = TRUE, textInput("id"))
  expect_snapshot(error = TRUE, textInput())
  expect_snapshot(error = TRUE, textAreaInput("id"))
  expect_snapshot(error = TRUE, passwordInput("id"))
  expect_snapshot(error = TRUE, numericInput("id", "Label"))
  expect_snapshot(error = TRUE, checkboxInput("id"))
  expect_snapshot(error = TRUE, checkboxGroupInput("id"))
  expect_snapshot(error = TRUE, radioButtons("id"))
  expect_snapshot(error = TRUE, selectInput("id", "Label"))
  expect_snapshot(error = TRUE, varSelectInput("id", "Label"))
  expect_snapshot(error = TRUE, dateInput("id"))
  expect_snapshot(error = TRUE, dateRangeInput("id"))
  expect_snapshot(error = TRUE, fileInput("id"))
  expect_snapshot(error = TRUE, sliderInput("id", "Label", min = 1, max = 10))
  expect_snapshot(error = TRUE, sliderInput("id"))
  expect_snapshot(error = TRUE, actionButton("id"))
  expect_snapshot(error = TRUE, actionLink("id"))
})
