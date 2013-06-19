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


test_that("Adding child tags", {
  tag_list <- list(tags$p("tag1"), tags$b("tag2"), tags$i("tag3"))

  # Creating nested tags by calling the tag$div function and passing a list
  t1 <- tags$div(class="foo", tag_list)
  expect_equal(length(t1$children), 3)
  expect_equal(t1$children[[1]]$name, "p")
  expect_equal(t1$children[[1]]$children[[1]], "tag1")
  expect_equal(t1$children[[2]]$name, "b")
  expect_equal(t1$children[[2]]$children[[1]], "tag2")
  expect_equal(t1$children[[3]]$name, "i")
  expect_equal(t1$children[[3]]$children[[1]], "tag3")


  # div tag used as starting point for tests below
  div_tag <- tags$div(class="foo")

  # Appending each child
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChild(t2, tag_list[[2]])
  t2 <- tagAppendChild(t2, tag_list[[3]])
  expect_identical(t1, t2)


  # tagSetChildren, using list argument
  t2 <- tagSetChildren(div_tag, list = tag_list)
  expect_identical(t1, t2)

  # tagSetChildren, using ... arguments
  t2 <- tagSetChildren(div_tag, tag_list[[1]], tag_list[[2]], tag_list[[3]])
  expect_identical(t1, t2)

  # tagSetChildren, using ... and list arguments
  t2 <- tagSetChildren(div_tag, tag_list[[1]], list = tag_list[2:3])
  expect_identical(t1, t2)

  # tagSetChildren overwrites existing children
  t2 <- tagAppendChild(div_tag, p("should replace this tag"))
  t2 <- tagSetChildren(div_tag, list = tag_list)
  expect_identical(t1, t2)


  # tagAppendChildren, using list argument
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, list = tag_list[2:3])
  expect_identical(t1, t2)

  # tagAppendChildren, using ... arguments
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, tag_list[[2]], tag_list[[3]])
  expect_identical(t1, t2)

  # tagAppendChildren, using ... and list arguments
  t2 <- tagAppendChild(div_tag, tag_list[[1]])
  t2 <- tagAppendChildren(t2, tag_list[[2]], list = list(tag_list[[3]]))
  expect_identical(t1, t2)

  # tagAppendChildren can start with no children
  t2 <- tagAppendChildren(div_tag, list = tag_list)
  expect_identical(t1, t2)
})
