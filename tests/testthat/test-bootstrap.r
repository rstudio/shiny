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

  # Radio buttons using choices
  x <- radioButtons('id','label', choices = c(a='x1', a='x2', b='x3'))
  choices <- x$children

  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[2]]$children[[1]], HTML('a'))
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$value, 'x1')
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$checked, 'checked')

  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[2]]$children[[1]], HTML('a'))
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$value, 'x2')
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$checked, NULL)

  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[2]]$children[[1]], HTML('b'))
  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[1]]$attribs$value, 'x3')
  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[1]]$attribs$checked, NULL)

  # Radio buttons using choiceNames and choiceValues
  x <- radioButtons('id','label',
    choiceNames = list(icon('calendar'), HTML('<p style="color:red;">Red</p>'), 'Normal'),
    choiceValues = list('icon', 'html', 'text')
  )
  choices <- x$children

  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[2]]$children[[1]],
    HTML('<i class="fa fa-calendar"></i>'))
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$value, 'icon')
  expect_equal(choices[[2]]$children[[1]][[1]]$children[[1]]$children[[1]]$attribs$checked, 'checked')

  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[2]]$children[[1]],
    HTML('<p style="color:red;">Red</p>'))
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$value, 'html')
  expect_equal(choices[[2]]$children[[1]][[2]]$children[[1]]$children[[1]]$attribs$checked, NULL)

  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[2]]$children[[1]],
    HTML('Normal'))
  expect_equal(choices[[2]]$children[[1]][[3]]$children[[1]]$children[[1]]$attribs$value, 'text')
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
    list(a="a", b="b", "3"="3")
  )
  # Vector, with some named, some not
  expect_identical(
    choicesWithNames(c(A="a", "b", C="3", "4")),
    list(A="a", "b"="b", C="3", "4"="4")
  )
  # List, with some named, some not
  expect_identical(
    choicesWithNames(list(A="a", "b", C=3, 4)),
    list(A="a", "b"="b", C="3", "4"="4")
  )
  # List, named, with a sub-vector
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with a sub-vector with numeric elements
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(1, 2))),
    list(A="a", B="b", C=list(`1`="1", `2`="2"))
  )
  # List, named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=list("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with sublist with numeric elements
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=list(1, 2))),
    list(A="a", B="b", C=list(`1`="1", `2`="2"))
  )
  # List, named, with a named sub-vector of length 1
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D="d"))),
    list(A="a", B="b", C=list(D="d"))
  )
  # List, named, with a named sub-vector of length 1 with a numeric element
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D=1))),
    list(A="a", B="b", C=list(D="1"))
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

test_that("normalizeChoicesArgs does its job", {

  # Unnamed vectors and lists
  expected <- list(choiceNames = list("a", "b"), choiceValues = list("a", "b"))
  expect_equal(normalizeChoicesArgs(c("a", "b"), NULL, NULL), expected)
  expect_equal(normalizeChoicesArgs(list("a", "b"), NULL, NULL), expected)

  # Named list
  expected <- list(choiceNames = list("one", "two"), choiceValues = list("a", "b"))
  x <- list(one = "a", two = "b")
  expect_equal(normalizeChoicesArgs(x, NULL, NULL), expected)
  expect_equal(normalizeChoicesArgs(NULL, names(x), unname(x)), expected)

  # Using unnamed `choiceNames` and `choiceValues` vectors/lists directly
  expect_equal(normalizeChoicesArgs(NULL, c("one", "two"),  c("a", "b")), expected)
  expect_equal(normalizeChoicesArgs(NULL, list("one", "two"), list("a", "b")), expected)

  # Numbers
  expected <- list(choiceNames = list("a", "b"), choiceValues = list("1", "2"))
  expect_equal(normalizeChoicesArgs(c("a" = 1, "b" = 2), NULL, NULL), expected)
  expect_equal(normalizeChoicesArgs(list("a" = 1, "b" = 2), NULL, NULL), expected)
  expect_equal(normalizeChoicesArgs(NULL, c("a", "b"), c(1, 2)), expected)
  expect_equal(normalizeChoicesArgs(NULL, list("a", "b"), list("1", "2")), expected)

  # Using choiceNames with HTML and choiceValues
  nms <- list(icon("calendar"), HTML("<p style='color:red;'>Red Text</p>"))
  vals <- list("a", "b")
  expected <- list(choiceNames = nms, choiceValues = vals)
  expect_equal(normalizeChoicesArgs(NULL, nms, vals), expected)

  # Attempt to use choices, AND choiceNames + choiceValues
  x <- list("a", "b")
  expect_warning(res <- normalizeChoicesArgs(x, nms, vals),
    "Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
  expect_equal(res, list(choiceNames = list("a", "b"), choiceValues = list("a", "b")))

  # Set possibilities to character(0)
  expected <- list(choiceNames = list(), choiceValues = list())
  expect_equal(normalizeChoicesArgs(character(0), NULL, NULL), expected)
  expect_equal(normalizeChoicesArgs(NULL, character(0), character(0)), expected)
  expect_warning(res <- normalizeChoicesArgs(character(0), character(0), character(0)),
    "Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
  expect_equal(res, expected)

  # Set possibilities to NULL in an inconsistent way
  expected <- paste("One of `choiceNames` or `choiceValues` was set to NULL,",
                    "but either both or none should be NULL.")
  expect_error(normalizeChoicesArgs(NULL, character(0), NULL, FALSE), expected, fixed = TRUE)
  expect_error(normalizeChoicesArgs(NULL, NULL, character(0), FALSE), expected, fixed = TRUE)
  expected <- paste("Please specify a non-empty vector for `choices` (or,",
                    "alternatively, for both `choiceNames` AND `choiceValues`).")
  expect_error(normalizeChoicesArgs(NULL, character(0), NULL), expected, fixed = TRUE)
  expect_error(normalizeChoicesArgs(NULL, NULL, character(0)), expected, fixed = TRUE)

  # Set all possibilities to NULL (and mustExist = FALSE)
  expected <- list(choiceNames = NULL, choiceValues = NULL)
  expect_equal(normalizeChoicesArgs(NULL, NULL, NULL, FALSE), expected)
})

test_that("Choices need not be provided, can be NULL or c()", {

  expected <- "<div id=\"cb\" class=\"form-group shiny-input-checkboxgroup shiny-input-container\">\n  <label class=\"control-label\" for=\"cb\">Choose:</label>\n  <div class=\"shiny-options-group\"></div>\n</div>"
  noChoices <- checkboxGroupInput("cb", "Choose:")
  choicesNull <- checkboxGroupInput("cb", "Choose:", choices = NULL)
  choicesCharacter <- checkboxGroupInput("cb", "Choose:", choices = c())
  choicesCharacter0 <- checkboxGroupInput("cb", "Choose:", choices = character(0))
  allChoicesNull <- checkboxGroupInput("cb", "Choose:", choices = NULL,
    choiceNames = NULL, choiceValues = NULL)

  expect_identical(noChoices, choicesNull)
  expect_identical(noChoices, choicesCharacter)
  expect_identical(noChoices, choicesCharacter0)
  expect_identical(noChoices, allChoicesNull)

  expect_true(grepl(fixed = TRUE, expected, format(noChoices)))
})
