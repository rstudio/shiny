context("UI")

test_that("selectInput options are properly escaped", {
  si <- selectInput("quote", "Quote", list(
    "\"Separators\"" = list(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )
  ))

  si_str <- as.character(si)
  expect_true(any(grepl("<option value=\"&quot;\">", si_str, fixed = TRUE)))
  expect_true(any(grepl("<option value=\"&#39;\">", si_str, fixed = TRUE)))
  expect_true(any(grepl("<optgroup label=\"&quot;Separators&quot;\">", si_str, fixed = TRUE)))
})


# For issue #1006
test_that("sliderInput steps don't have rounding errors", {
  # Need to use expect_identical; expect_equal is too forgiving of rounding error
  expect_identical(findStepSize(-5.5, 4, NULL), 0.1)
})


test_that("selectInputUI has a select at an expected location", {
  for (multiple in c(TRUE, FALSE)) {
    for (selected in list(NULL, "", "A")) {
      for (selectize in c(TRUE, FALSE)) {
        selectInputVal <- selectInput(
          inputId = "testId",
          label = "test label",
          choices = c("A", "B", "C"),
          selected = selected,
          multiple = multiple,
          selectize = selectize
        )
        # if this getter is changed, varSelectInput getter needs to be changed
        selectHtml <- selectInputVal$children[[2]]$children[[1]]
        expect_true(inherits(selectHtml, "shiny.tag"))
        expect_equal(selectHtml$name, "select")
        if (!is.null(selectHtml$attribs$class)) {
          expect_false(grepl(selectHtml$attribs$class, "symbol"))
        }

        varSelectInputVal <- varSelectInput(
          inputId = "testId",
          label = "test label",
          data = data.frame(A = 1:2, B = 3:4, C = 5:6),
          selected = selected,
          multiple = multiple,
          selectize = selectize
        )
        # if this getter is changed, varSelectInput getter needs to be changed
        varSelectHtml <- varSelectInputVal$children[[2]]$children[[1]]
        expect_true(inherits(varSelectHtml, "shiny.tag"))
        expect_equal(varSelectHtml$name, "select")
        expect_true(grepl("symbol", varSelectHtml$attribs$class, fixed = TRUE))
      }
    }
  }
})
