

test_that("tabPanelBody validates it's input", {
  expect_silent(tabPanelBody("a", "content1", "content2", icon = icon("table")))
  expect_silent(tabPanelBody(value = "a", "content1", "content2", icon = icon("table")))

  expect_error(tabPanelBody())
  expect_error(tabPanelBody(NULL), "single, non-empty string")
  expect_error(tabPanelBody(1), "single, non-empty string")
  expect_error(tabPanelBody(TRUE), "single, non-empty string")
  expect_error(tabPanelBody(NA), "single, non-empty string")
  expect_error(tabPanelBody(NA_character_), "single, non-empty string")
  expect_error(tabPanelBody(""), "single, non-empty string")
  expect_error(tabPanelBody(letters[1:2]), "single, non-empty string")
})
