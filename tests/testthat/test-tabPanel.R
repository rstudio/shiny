
context("tabPanel")

test_that("tabPanelBody", {
  expect_error(tabPanelBody(), "at least one character")
  expect_error(tabPanelBody(NULL), "at least one character")
  expect_error(tabPanelBody(1), "at least one character")
  expect_error(tabPanelBody(TRUE), "at least one character")
  expect_error(tabPanelBody(NA), "at least one character")
  expect_error(tabPanelBody(NA_character_), "at least one character")
  expect_error(tabPanelBody(""), "at least one character")
  expect_error(tabPanelBody(letters[1:2]), "at least one character")
  expect_silent(tabPanelBody("a"))
})
