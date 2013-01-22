context("tags")

test_that("Basic tag writing works", {
  expect_equal(as.character(tagList("hi")), HTML("hi"))
  expect_equal(
    as.character(tagList("one", "two", tagList("three"))),
    HTML("one\ntwo\nthree"))
  expect_equal(
    as.character(tags$b("one")),
    HTML("<b>one</b>"))
  expect_equal(
    as.character(tags$b("one", "two")),
    HTML("<b>\n  one\n  two\n</b>"))
  expect_equal(
    as.character(tagList(list("one"))),
    HTML("one"))
  expect_equal(
    as.character(tagList(list(tagList("one")))),
    HTML("one"))
  expect_equal(
    as.character(tagList(tags$br(), "one")),
    HTML("<br/>\none"))
})
