context("inline-markdown")

test_that("Markdown without newlines translates", {
  expect_equivalent(markdown("# a top level"), HTML("<h1>a top level</h1>\n"))
  expect_equivalent(markdown("## a subheading"), HTML("<h2>a subheading</h2>\n"))
  expect_equivalent(markdown("[rstudio](https://rstudio.com)"), HTML("<p><a href=\"https://rstudio.com\">rstudio</a></p>\n"))
})

test_that("HTML has correct attributes", {
  html <- markdown("a paragraph", .noWS = "outside")
  expect_is(html, "html")
  expect_equal(attr(html, "noWS"), "outside")
})

test_that("Github extensions are on by default", {
  html <- markdown("a ~paragraph~ with a link: https://example.com")
  expect_equivalent(html, HTML("<p>a <del>paragraph</del> with a link: <a href=\"https://example.com\">https://example.com</a></p>\n"))
})

test_that("Github extensions can be disabled", {
  html <- markdown("a ~paragraph~", extensions = FALSE)
  expect_equivalent(html, HTML("<p>a ~paragraph~</p>\n"))
})

test_that("Additional options are respected", {
  html <- markdown("a ~paragraph~", extensions = FALSE, sourcepos = TRUE)
  expect_equivalent(html, HTML("<p data-sourcepos=\"1:1-1:13\">a ~paragraph~</p>\n"))
})

test_that("Multiline markdown works properly", {
})
