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
  expect_match(
    format(x),
    '<select class="shiny-input-select form-control" id="id"><option value="x1" selected>a</option>\n<option value="x2">a</option>\n<option value="x3">b</option></select>',
    fixed = TRUE
  )

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
    HTML('<i class="far fa-calendar" role="presentation" aria-label="calendar icon"></i>'))
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
  # Empty non-list comes back as a list with names
  expect_identical(
    choicesWithNames(numeric(0)),
    stats::setNames(list(), character(0))
  )
  # Empty list comes back with names
  expect_identical(
    choicesWithNames(list()),
    stats::setNames(list(), character(0))
  )
  # NULL comes back as an empty list with names
  expect_identical(
    choicesWithNames(NULL),
    stats::setNames(list(), character(0))
  )
  # NA is processed as a leaf, not a group
  expect_identical(
    choicesWithNames(NA),
    as.list(stats::setNames(as.character(NA), NA))
  )
  # Empty character vector
  # An empty character vector isn't a sensical input, but we preserved this test
  # in the off chance that somebody relies on the existing behavior.
  expect_identical(
    choicesWithNames(c("")),
    stats::setNames(list(""), "")
  )
  # Single-item character vector
  expect_identical(
    choicesWithNames(c("foob")),
    list(foob="foob")
  )
  # Unnamed character vector
  expect_identical(
    choicesWithNames(c("a","b","3")),
    list(a="a", b="b", "3"="3")
  )
  # Unnamed numeric vector
  expect_identical(
    choicesWithNames(c(1,2,3)),
    list(`1`="1", `2`="2", `3`="3")
  )
  # Unnamed list
  expect_identical(
    choicesWithNames(list("a","b",3)),
    list(a="a", b="b", "3"="3")
  )
  # Complex vector, with some named, some not
  expect_identical(
    choicesWithNames(c(A=Inf+0i, 1+0i, C=0+0i)),
    list(A="Inf+0i", "1+0i"="1+0i", C="0+0i")
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
  # List, named, with a named sub-vector of length 1 with a logical element
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D=TRUE))),
    list(A="a", B="b", C=list(D="TRUE"))
  )
  # List, some named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", "b", C=list("d", E="e"))),
    list(A="a", b="b", C=list(d="d", E="e"))
  )
  # List, with a single-item unnamed group list
  expect_identical(
    choicesWithNames(list(C=list(123))),
    list(C=list("123"="123"))
  )
  # Error when sublist is unnamed
  expect_error(choicesWithNames(list(A="a", "b", list(1,2))))
  # Error when list is unnamed and contains a group
  # NULL, list(1,2), and anything of length() == 0 is considered a group.
  # NA is NOT a group.
  expect_error(choicesWithNames(list(NULL)), regexp = "must be named")
  expect_error(choicesWithNames(list(list(1,2))), regexp = "must be named")
  expect_error(choicesWithNames(list(character(0))), regexp = "must be named")
  # Unnamed factor
  expect_identical(
    choicesWithNames(factor(c("a","b","3"))),
    list(a="a", b="b", "3"="3")
  )
  # Named factor
  expect_identical(
    choicesWithNames(structure(factor(c("foo", "bar")), names = c("A", "B"))),
    list(A="foo", B="bar")
  )
  # List, named, with a sub-factor
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=factor(c("d", "e")))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with a named sub-factor
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=structure(factor(c("d", "e")), names = c("d", "e")))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with an empty group as an unnamed empty list
  expect_identical(
    choicesWithNames(list(C=list())),
    list(C=stats::setNames(list(), character()))
  )
  # List, named, with an empty group as an unnamed empty vector
  expect_identical(
    choicesWithNames(list(C=c())),
    list(C=stats::setNames(list(), character()))
  )
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
  expect_match(
    format(selectInput('x', NULL, NULL, selectize = FALSE)),
    '<select class="shiny-input-select form-control" id="x"></select>',
    fixed = TRUE
  )

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

  expected <- '<div id="cb" class="form-group shiny-input-checkboxgroup shiny-input-container" role="group" aria-labelledby="cb-label">\n  <label class="control-label" id="cb-label" for="cb">Choose:</label>\n  <div class="shiny-options-group"></div>\n</div>'
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

  expect_equal(expected, format(noChoices))
})

# https://github.com/rstudio/shiny/pull/3187
test_that("radioButtons() and checkboxGroupInput() accessibility", {
  rb <- radioButtons("foo", "bar", c("a", "b"))
  rb_lbl <- rb$children[[1]]
  expect_equal(rb$attribs$role, "radiogroup")
  expect_equal(rb_lbl$name, "label")
  expect_true(!is.null(rb_lbl$attribs$id))
  expect_equal(
    rb$attribs$`aria-labelledby`, rb_lbl$attribs$id
  )

  cbg <- checkboxGroupInput("foo", "bar", c("a", "b"))
  cbg_lbl <- cbg$children[[1]]
  expect_equal(cbg$attribs$role, "group")
  expect_equal(cbg_lbl$name, "label")
  expect_true(!is.null(cbg_lbl$attribs$id))
  expect_equal(
    cbg$attribs$`aria-labelledby`, cbg_lbl$attribs$id
  )
})
