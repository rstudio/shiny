
# No non-reactive functions in our app to test, so we'll just use a placeholder.
test_that("fake function works", {
  expect_equal(1,1)

  # just prove that the counter helper module was loaded into this environment and is available.
  expect_type(counter, "closure")
})
