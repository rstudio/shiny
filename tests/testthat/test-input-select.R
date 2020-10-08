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
