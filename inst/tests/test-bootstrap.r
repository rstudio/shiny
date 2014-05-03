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
  expect_equal(format(x), '<label class="control-label" for="id">label</label>
<select id="id"><option value="x1" selected>a</option>\n<option value="x2">a</option>\n<option value="x3">b</option></select>')


  # Radio buttons
  x <- radioButtons('id','label', choices = c(a='x1', a='x2', b='x3'))
  choices <- x$children

  expect_equal(choices[[2]][[1]]$children[[2]]$children[[1]], 'a')
  expect_equal(choices[[2]][[1]]$children[[1]]$attribs$value, 'x1')
  expect_equal(choices[[2]][[1]]$children[[1]]$attribs$checked, 'checked')

  expect_equal(choices[[2]][[2]]$children[[2]]$children[[1]], 'a')
  expect_equal(choices[[2]][[2]]$children[[1]]$attribs$value, 'x2')
  expect_equal(choices[[2]][[2]]$children[[1]]$attribs$checked, NULL)

  expect_equal(choices[[2]][[3]]$children[[2]]$children[[1]], 'b')
  expect_equal(choices[[2]][[3]]$children[[1]]$attribs$value, 'x3')
  expect_equal(choices[[2]][[3]]$children[[1]]$attribs$checked, NULL)
})
