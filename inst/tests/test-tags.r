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


test_that("withTags works", {
  output_tags <- tags$div(class = "myclass",
    tags$h3("header"),
    tags$p("text here")
  )
  output_withhtml <- withTags(
    div(class = "myclass",
      h3("header"),
      p("text here")
    )
  )
  expect_identical(output_tags, output_withhtml)


  # Check that current environment is searched
  x <- 100
  expect_identical(tags$p(x), withTags(p(x)))

  # Just to make sure, run it in a function, which has its own environment
  foo <- function() {
    y <- 100
    withTags(p(y))
  }
  expect_identical(tags$p(100), foo())
})
