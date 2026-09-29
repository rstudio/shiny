test_that("frozen values are excluded from bookmark serialization", {
  session <- MockShinySession$new()
  session$setInputs(a = 1, b = 2)

  isolate(session$freezeValue(session$input, "b"))

  vals <- isolate(serializeReactiveValues(session$input, exclude = character(0)))
  expect_equal(vals, list(a = 1))
})

test_that("non-frozen values still serialize normally", {
  session <- MockShinySession$new()
  session$setInputs(x = 10, y = 20)

  vals <- isolate(serializeReactiveValues(session$input, exclude = character(0)))
  expect_equal(vals, list(x = 10, y = 20))
})
