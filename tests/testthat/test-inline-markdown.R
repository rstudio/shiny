context("inline-markdown")

test_that("Markdown without newlines is converted properly", {
  expect_equivalent(markdown("# a top level"), HTML("<h1>a top level</h1>"))
})
