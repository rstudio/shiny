test_that("performance warning works", {
  pattern <- "consider using server-side selectize"

  expect_warning(selectInput("x", "x", as.character(1:999)), NA)
  expect_warning(selectInput("x", "x", as.character(1:999), selectize = TRUE), NA)
  expect_warning(selectInput("x", "x", as.character(1:999), selectize = FALSE), NA)
  expect_warning(selectizeInput("x", "x", as.character(1:999)), NA)

  expect_warning(selectInput("x", "x", as.character(1:1000)), pattern)
  expect_warning(selectInput("x", "x", as.character(1:1000), selectize = TRUE), pattern)
  expect_warning(selectInput("x", "x", as.character(1:1000), selectize = FALSE), pattern)
  expect_warning(selectizeInput("x", "x", as.character(1:1000)), pattern)
  expect_warning(selectInput("x", "x", as.character(1:2000)), pattern)
  expect_warning(selectInput("x", "x", as.character(1:2000), selectize = TRUE), pattern)
  expect_warning(selectInput("x", "x", as.character(1:2000), selectize = FALSE), pattern)
  expect_warning(selectizeInput("x", "x", as.character(1:2000)), pattern)

  session <- MockShinySession$new()

  expect_warning(updateSelectInput(session, "x", choices = as.character(1:999)), NA)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:999)), NA)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:999), server = FALSE), NA)

  expect_warning(updateSelectInput(session, "x", choices = as.character(1:1000)), pattern)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:1000)), pattern)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:1000), server = FALSE), pattern)
  expect_warning(updateSelectInput(session, "x", choices = as.character(1:2000)), pattern)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:2000)), pattern)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:2000), server = FALSE), pattern)

  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:999), server = TRUE), NA)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:1000), server = TRUE), NA)
  expect_warning(updateSelectizeInput(session, "x", choices = as.character(1:2000), server = TRUE), NA)
})


test_that("jqueryui is attached when drag_drop plugin is present", {
  x <- selectizeInput("test", "test", choices = 1:3, multiple = TRUE, options = list(plugins = "drag_drop"))
  deps <- htmltools::resolveDependencies(htmltools::htmlDependencies(x))
  expect_length(deps, 2)
  expect_setequal(
    vapply(deps, `[[`, character(1), "name"),
    c("selectize", "jqueryui")
  )
})


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
