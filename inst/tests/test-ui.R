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
