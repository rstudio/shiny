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


test_that("HTML escaping in tags", {
  # Regular text is escaped
  expect_equivalent(format(div("<a&b>")), "<div>&lt;a&amp;b&gt;</div>")

  # Text in HTML() isn't escaped
  expect_equivalent(format(div(HTML("<a&b>"))), "<div><a&b></div>")

  # Text in a property is escaped
  expect_equivalent(format(div(class = "<a&b>", "text")),
                    '<div class="&lt;a&amp;b&gt;">text</div>')

  # HTML() has no effect in a property like 'class'
  expect_equivalent(format(div(class = HTML("<a&b>"), "text")),
                    '<div class="&lt;a&amp;b&gt;">text</div>')
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


  # tagSetChildren preserves attributes
  x <- tagSetChildren(div(), HTML("text"))
  expect_identical(attr(x$children[[1]], "html"), TRUE)

  # tagAppendChildren preserves attributes
  x <- tagAppendChildren(div(), HTML("text"))
  expect_identical(attr(x$children[[1]], "html"), TRUE)
})


test_that("Creating simple tags", {
  # Empty tag
  expect_identical(
    div(),
    structure(
      list(name = "div", attribs = list(), children = list()),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # Tag with text
  expect_identical(
    div("text"),
    structure(
      list(name = "div", attribs = list(), children = list("text")),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # NULL attributes are dropped
  expect_identical(
    div(a = NULL, b = "value"),
    div(b = "value")
  )

  # Numbers are coerced to strings
  expect_identical(
    div(1234),
    structure(
      list(name = "div", attribs = list(), children = list("1234")),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )
})


test_that("Creating nested tags", {
  # Simple version
  # Note that the $children list should not have a names attribute
  expect_identical(
    div(class="foo", list("a", "b")),
    structure(
      list(name = "div",
           attribs = structure(list(class = "foo"), .Names = "class"),
           children = list("a", "b")),
      .Names = c("name", "attribs", "children"),
      class = "shiny.tag"
    )
  )

  # More complex version
  t1 <- withTags(
    div(class = "foo",
      p("child tag"),
      list(
        p("in-list child tag 1"),
        "in-list character string",
        p(),
        p("in-list child tag 2")
      ),
      "character string",
      1234
    )
  )

  # t1 should be identical to this data structure.
  # The nested list should be flattened, and non-tag, non-strings should be
  # converted to strings
  t1_full <- structure(
    list(
      name = "div",
      attribs = list(class = "foo"),
      children = list(
        structure(list(name = "p",
                       attribs = list(),
                       children = list("child tag")),
                  class = "shiny.tag"
        ),
        structure(list(name = "p",
                       attribs = list(),
                       children = list("in-list child tag 1")),
                  class = "shiny.tag"
        ),
        "in-list character string",
        structure(list(name = "p",
                       attribs = list(),
                       children = list()),
                  class = "shiny.tag"
        ),
        structure(list(name = "p",
                       attribs = list(),
                       children = list("in-list child tag 2")),
                  class = "shiny.tag"
        ),
        "character string",
        "1234"
      )
    ),
    class = "shiny.tag"
  )

  expect_identical(t1, t1_full)
})

test_that("Attributes are preserved", {
  # HTML() adds an attribute to the data structure (note that this is
  # different from the 'attribs' field in the list)
  x <- HTML("<tag>&&</tag>")
  expect_identical(attr(x, "html"), TRUE)
  expect_equivalent(format(x), "<tag>&&</tag>")

  # Make sure attributes are preserved when wrapped in other tags
  x <- div(HTML("<tag>&&</tag>"))
  expect_equivalent(x$children[[1]], HTML("<tag>&&</tag>"))
  expect_identical(attr(x$children[[1]], "html"), TRUE)
  expect_equivalent(format(x), "<div><tag>&&</tag></div>")

  # Deeper nesting
  x <- div(p(HTML("<tag>&&</tag>")))
  expect_equivalent(x$children[[1]]$children[[1]], HTML("<tag>&&</tag>"))
  expect_identical(attr(x$children[[1]]$children[[1]], "html"), TRUE)
  expect_equivalent(format(x), "<div>\n  <p><tag>&&</tag></p>\n</div>")
})


test_that("Flattening a list of tags", {
  # Flatten a nested list
  nested <- list(
    "a1",
    list(
      "b1",
      list("c1", "c2"),
      list(),
      "b2",
      list("d1", "d2")
    ),
    "a2"
  )
  flat <- list("a1", "b1", "c1", "c2", "b2", "d1", "d2", "a2")
  expect_identical(flattenTags(nested), flat)

  # no-op for flat lists
  expect_identical(flattenTags(list(a="1", "b")), list(a="1", "b"))

  # numbers are coerced to character
  expect_identical(flattenTags(list(a=1, "b")), list(a="1", "b"))

  # empty list results in empty list
  expect_identical(flattenTags(list()), list())

  # preserve attributes
  nested <- list("txt1", list(structure("txt2", prop="prop2")))
  flat <- list("txt1",
               structure("txt2", prop="prop2"))
  expect_identical(flattenTags(nested), flat)
})

test_that("Head and singleton behavior", {
  result <- renderTags(tagList(
    tags$head(singleton("hello"))
  ))
  
  expect_identical(result$html, HTML(""))
  expect_identical(result$head, HTML("  hello"))
  expect_identical(result$singletons, "60eed8231e688bcba7c275c58dd2e3b4dacb61f0")
  
  # Ensure that "hello" actually behaves like a singleton
  result2 <- renderTags(tagList(
    tags$head(singleton("hello"))
  ), singletons = result$singletons)
  
  expect_identical(result$singletons, result2$singletons)
  expect_identical(result2$head, HTML(""))
  expect_identical(result2$html, HTML(""))
})