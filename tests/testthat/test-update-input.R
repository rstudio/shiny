test_that("Radio buttons and checkboxes work with modules", {
  createModuleSession <- function(moduleId) {
    session <- as.environment(list(
      ns = NS(moduleId),
      sendInputMessage = function(inputId, message) {
        session$lastInputMessage = list(id = inputId, message = message)
      }
    ))
    class(session) <- "ShinySession"
    session
  }

  sessA <- createModuleSession("modA")

  updateRadioButtons(sessA, "test1", label = "Label", choices = letters[1:5])
  resultA <- sessA$lastInputMessage

  expect_equal("test1", resultA$id)
  expect_equal("Label", as.character(resultA$message$label$html))
  expect_equal("a", resultA$message$value)
  expect_true(grepl('"modA-test1"', resultA$message$options))
  expect_false(grepl('"test1"', resultA$message$options))


  sessB <- createModuleSession("modB")

  updateCheckboxGroupInput(sessB, "test2", label = icon("eye"), choices = LETTERS[1:5])
  resultB <- sessB$lastInputMessage

  expect_equal("test2", resultB$id)
  expect_length(resultB$message$label, 2)
  expect_s3_class(resultB$message$label$html, "html")
  expect_null(resultB$message$value)
  expect_true(grepl('"modB-test2"', resultB$message$options))
  expect_false(grepl('"test2"', resultB$message$options))
})
