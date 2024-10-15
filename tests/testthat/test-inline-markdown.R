test_that("Markdown without newlines translates", {
  expect_equal(markdown("# a top level"), HTML("<h1>a top level</h1>\n"))
  expect_equal(markdown("## a subheading"), HTML("<h2>a subheading</h2>\n"))
  expect_equal(markdown("[rstudio](https://rstudio.com)"), HTML("<p><a href=\"https://rstudio.com\">rstudio</a></p>\n"))
})

test_that("HTML has correct attributes", {
  html <- markdown("a paragraph", .noWS = "outside")
  expect_s3_class(html, "html")
  expect_equal(attr(html, "noWS"), "outside")
})

test_that("Github extensions are on by default", {
  html <- markdown("a ~~paragraph~~ with a link: https://example.com")
  expect_equal(html, HTML("<p>a <del>paragraph</del> with a link: <a href=\"https://example.com\">https://example.com</a></p>\n"))
})

test_that("Github extensions can be disabled", {
  html <- markdown("a ~~paragraph~~", extensions = FALSE)
  expect_equal(html, HTML("<p>a ~~paragraph~~</p>\n"))
})

test_that("Additional options are respected", {
  html <- markdown("a ~~paragraph~~", extensions = FALSE, sourcepos = TRUE)
  expect_equal(html, HTML("<p data-sourcepos=\"1:1-1:15\">a ~~paragraph~~</p>\n"))
})

test_that("Multiline markdown works properly", {
  essay <- paste0("
   # The [Louisiana Purchase](https://en.wikipedia.org/wiki/Louisiana_Purchase)\n",
   "   ", "
   Larry Sellers
   Mrs. Jamtoss
   History Period 4

   ## Introduction

   The most important purchase in history is the Lousiana
   Purchase. It was also the most important evente. It
   happened in President Jeffersons 1st administration.
   Its when the United States bought 827,987 square miles
   of lande from the French guys.

   The end.")

  expect_equal(strsplit(essay, "\n")[[1]][[3]], "   ")

  expected <- HTML(paste0(c(
    "<h1>The <a href=\"https://en.wikipedia.org/wiki/Louisiana_Purchase\">Louisiana Purchase</a></h1>",
    "<p>Larry Sellers",
    "Mrs. Jamtoss",
    "History Period 4</p>",
    "<h2>Introduction</h2>",
    "<p>The most important purchase in history is the Lousiana",
    "Purchase. It was also the most important evente. It",
    "happened in President Jeffersons 1st administration.",
    "Its when the United States bought 827,987 square miles",
    "of lande from the French guys.</p>",
    "<p>The end.</p>",
    ""
  ), collapse = "\n"))
  expect_equal(markdown(essay), expected)
})
