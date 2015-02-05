context("bootstrap")

test_that("CSS unit validation", {
  # On error, return NA; on success, return result
  validateCssUnit_wrap <- function(x) {
    tryCatch(validateCssUnit(x), error = function(e) { NA_character_ })
  }

  # Test strings and expected results
  strings  <- c("100x", "10px", "10.4px", ".4px", "1px0", "px", "5",  "%", "5%", "auto", "1auto", "")
  expected <- c(NA,     "10px", "10.4px", ".4px", NA,       NA, "5px", NA, "5%", "auto", NA,      NA)
  results <- vapply(strings, validateCssUnit_wrap, character(1), USE.NAMES = FALSE)
  expect_equal(results, expected)

  # Numbers should return string with "px"
  expect_equal(validateCssUnit(100), "100px")
})


test_that("Repeated names for selectInput and radioButtons choices", {
  # These test might be a bit too closely tied to the exact structure of the
  # tag object, but they get the job done for now.

  # Select input
  x <- selectInput('id','label', choices = c(a='x1', a='x2', b='x3'), selectize = FALSE)
  expect_true(grepl(fixed = TRUE,
    '<select id="id" class="form-control"><option value="x1" selected>a</option>\n<option value="x2">a</option>\n<option value="x3">b</option></select>',
     format(x)
  ))


  # Radio buttons
  x <- radioButtons('id','label', choices = c(a='x1', a='x2', b='x3'))
  choices <- x$children

  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[2]]$children[[1]], 'a')
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$value, 'x1')
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$checked, 'checked')

  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[2]]$children[[1]], 'a')
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$value, 'x2')
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$checked, NULL)

  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[2]]$children[[1]], 'b')
  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[1]]$attribs$value, 'x3')
  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[1]]$attribs$checked, NULL)
})


test_that("Choices are correctly assigned names", {
  # Unnamed vector
  expect_identical(
    choicesWithNames(c("a","b","3")),
    list(a="a", b="b", "3"="3")
  )
  # Unnamed list
  expect_identical(
    choicesWithNames(list("a","b",3)),
    list(a="a", b="b", "3"=3)
  )
  # Vector, with some named, some not
  expect_identical(
    choicesWithNames(c(A="a", "b", C="3", "4")),
    list(A="a", "b"="b", C="3", "4"="4")
  )
  # List, with some named, some not
  expect_identical(
    choicesWithNames(list(A="a", "b", C=3, 4)),
    list(A="a", "b"="b", C=3, "4"=4)
  )
  # List, named, with a sub-vector
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=list("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with a named sub-vector of length 1
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D="d"))),
    list(A="a", B="b", C=list(D="d"))
  )
  # List, some named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", "b", C=list("d", E="e"))),
    list(A="a", b="b", C=list(d="d", E="e"))
  )
  # Deeper nesting
  expect_identical(
    choicesWithNames(list(A="a", "b", C=list(D=list("e", "f"), G=c(H="h", "i")))),
    list(A="a", b="b", C=list(D=list(e="e", f="f"), G=list(H="h", i="i")))
  )
  # Error when sublist is unnamed
  expect_error(choicesWithNames(list(A="a", "b", list(1,2))))
})


test_that("selectOptions returns correct HTML", {
  # None selected
  expect_identical(
    selectOptions(choicesWithNames(list("a", "b")), list()),
    HTML("<option value=\"a\">a</option>\n<option value=\"b\">b</option>")
  )
  # One selected
  expect_identical(
    selectOptions(choicesWithNames(list("a", "b")), "a"),
    HTML("<option value=\"a\" selected>a</option>\n<option value=\"b\">b</option>")
  )
  # One selected, with named items
  expect_identical(
    selectOptions(choicesWithNames(list(A="a", B="b")), "a"),
    HTML("<option value=\"a\" selected>A</option>\n<option value=\"b\">B</option>")
  )
  # Two selected, with optgroup
  expect_identical(
    selectOptions(choicesWithNames(list("a", B=list("c", D="d"))), c("a", "d")),
    HTML("<option value=\"a\" selected>a</option>\n<optgroup label=\"B\">\n<option value=\"c\">c</option>\n<option value=\"d\" selected>D</option>\n</optgroup>")
  )

  # Escape HTML in strings
  expect_identical(
    selectOptions(choicesWithNames(list("<A>"="a", B="b")), "a"),
    HTML("<option value=\"a\" selected>&lt;A&gt;</option>\n<option value=\"b\">B</option>")
  )
})

test_that("selectInput selects items by default", {
  # None specified as selected (defaults to first)
  expect_true(grepl(fixed = TRUE,
    '<option value="a" selected>',
    selectInput('x', 'x', list("a", "b"))
  ))

  # Nested list (optgroup)
  expect_true(grepl(fixed = TRUE,
    '<option value="a" selected>',
    selectInput('x', 'x', list(A=list("a", "b"), "c"))
  ))

  # Nothing selected when choices=NULL
  expect_true(grepl(fixed = TRUE,
    '<select id="x" class="form-control"></select>',
    format(selectInput('x', NULL, NULL, selectize = FALSE))
  ))

  # None specified as selected. With multiple=TRUE, none selected by default.
  expect_true(grepl(fixed = TRUE,
    '<option value="a">',
    selectInput('x', 'x', list("a", "b"), multiple = TRUE)
  ))
})
