test_that("Action button accepts class arguments", {
  make_button <- function(class) {
    if (missing(class)) {
      actionButton("id", "label")
    } else {
      actionButton("id", "label", class = class)
    }
  }
  act <- make_button()
  get_class <- function(act) {
    act_html <- format(act)
    regmatches(act_html, regexec("class=\"[^\"]\"", act_html))[[1]]
  }
  act_class <- get_class(act)
  expect_equal(
    get_class(make_button(NULL)), act_class
  )
  expect_equal(
    get_class(make_button(NA)), act_class
  )
  expect_equal(
    get_class(make_button("extra")), sub("\"$", " extra\"", act_class)
  )
  expect_equal(
    get_class(make_button("extra extra2")), sub("\"$", " extra extra2\"", act_class)
  )
})



test_that("Action link accepts class arguments", {
  make_link <- function(class) {
    if (missing(class)) {
      actionLink("id", "label")
    } else {
      actionLink("id", "label", class = class)
    }
  }
  act <- make_link()
  get_class <- function(act) {
    act_html <- format(act)
    regmatches(act_html, regexec("class=\"[^\"]\"", act_html))[[1]]
  }
  act_class <- get_class(act)
  expect_equal(
    get_class(make_link(NULL)), act_class
  )
  expect_equal(
    get_class(make_link(NA)), act_class
  )
  expect_equal(
    get_class(make_link("extra")), sub("\"$", " extra\"", act_class)
  )
  expect_equal(
    get_class(make_link("extra extra2")), sub("\"$", " extra extra2\"", act_class)
  )
})
