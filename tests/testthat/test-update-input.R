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

  expect_equal(resultA$id, "test1")
  expect_equal(as.character(resultA$message$label$html), "Label")
  expect_equal(resultA$message$value, "a")
  expect_match(resultA$message$options, '"modA-test1"')
  expect_no_match(resultA$message$options, '"test1"')

  sessB <- createModuleSession("modB")

  updateCheckboxGroupInput(sessB, "test2", label = icon("eye"), choices = LETTERS[1:5])
  resultB <- sessB$lastInputMessage

  expect_equal(resultB$id, "test2")
  expect_length(resultB$message$label, 2)
  expect_s3_class(resultB$message$label$html, "html")
  expect_null(resultB$message$value)
  expect_match(resultB$message$options, '"modB-test2"')
  expect_no_match(resultB$message$options, '"test2"')
})
