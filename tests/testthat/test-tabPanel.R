

test_that("tabPanelBody", {
  expect_silent(tabPanelBody("a"))

  expect_error(tabPanelBody())
  expect_error(tabPanelBody(NULL), "single, non-empty string")
  expect_error(tabPanelBody(1), "single, non-empty string")
  expect_error(tabPanelBody(TRUE), "single, non-empty string")
  expect_error(tabPanelBody(NA), "single, non-empty string")
  expect_error(tabPanelBody(NA_character_), "single, non-empty string")
  expect_error(tabPanelBody(""), "single, non-empty string")
  expect_error(tabPanelBody(letters[1:2]), "single, non-empty string")
})

test_that("tabPanel works with deprecated interface", {
  title <- "titleVal"
  id <- "idVal"
  content <- "contentVal"

  expect_silent({
    test_val <- tabPanel(title, content, id = id)
  })
  expect_equal(
    expect_message(tabPanel(title, content, value = id), "is deprecated"),
    test_val
  )
})
