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

test_that("updateSelectizeInput(server = TRUE) handles empty choices, single-item optgroups, and tag labels", {
  registered <- list()
  sent <- list()
  sess <- as.environment(list(
    ns = identity,
    registerDataObj = function(name, data, filterFunc) {
      registered[[name]] <<- list(data = data, filterFunc = filterFunc)
      paste0("dataobj/", name)
    },
    sendInputMessage = function(inputId, message) {
      sent[[inputId]] <<- message
    }
  ))
  class(sess) <- "ShinySession"

  updateSelectizeInput(
    sess, "sel_empty",
    label = tags$span("My Label"),
    choices = character(0),
    server = TRUE
  )
  expect_s3_class(sent$sel_empty$label$html, "html")
  expect_equal(nrow(registered$sel_empty$data), 0L)
  expect_equal(colnames(registered$sel_empty$data), c("label", "value"))

  updateSelectizeInput(
    sess, "sel_group",
    choices = list(GroupA = c(One = "1"), GroupB = list()),
    server = TRUE
  )
  expect_equal(
    registered$sel_group$data,
    data.frame(
      label = "One",
      value = "1",
      optgroup = "GroupA",
      stringsAsFactors = FALSE
    )
  )

  # selectizeJSON handles leading whitespace in query and 1-column data frames
  res <- selectizeJSON(
    data.frame(value = c("alpha", "beta"), stringsAsFactors = FALSE),
    list(QUERY_STRING = "?query=%20alpha&field=%5B%22value%22%5D&value=value&conju=and&maxop=10")
  )
  expect_equal(res$status, 200L)
  expect_match(res$content, "alpha")
  expect_no_match(res$content, "beta")
})
