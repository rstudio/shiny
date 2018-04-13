context("text")

test_that("renderPrint and renderText behavior is correct", {
  expect_equal(isolate(renderPrint({ "foo" })()),
               '[1] "foo"')
  expect_equal(isolate(renderPrint({ invisible("foo") })()),
               '')
  expect_equal(isolate(renderPrint({ print("foo"); "bar"})()),
               '[1] "foo"\n[1] "bar"')
  expect_equal(isolate(renderPrint({ NULL })()),
               'NULL')
  expect_equal(isolate(renderPrint({ invisible() })()),
               '')
  expect_equal(isolate(renderPrint({ 1:5 })()),
               '[1] 1 2 3 4 5')

  expect_equal(isolate(renderText({ "foo" })()),
               'foo')
  expect_equal(isolate(renderText({ invisible("foo") })()),
               'foo')
  # Capture the print output so it's not shown on console during test, and
  # also check that it is correct
  print_out <- utils::capture.output(ret <- isolate(renderText({ print("foo"); "bar"})()))
  expect_equal(ret, 'bar')
  expect_equal(print_out, '[1] "foo"')
  expect_equal(isolate(renderText({ NULL })()),
               '')
  expect_equal(isolate(renderText({ invisible() })()),
               '')
  expect_equal(isolate(renderText({ 1:5 })()),
               '1 2 3 4 5')
})

test_that("reactive functions save visibility state", {
  # Call each function twice - should be no change in state with second call

  # invisible NULL
  f <- reactive({ invisible() })
  expect_identical(withVisible(isolate(f())), list(value=NULL, visible=FALSE))
  expect_identical(withVisible(isolate(f())), list(value=NULL, visible=FALSE))

  # visible NULL
  f <- reactive({ NULL })
  expect_identical(withVisible(isolate(f())), list(value=NULL, visible=TRUE))
  expect_identical(withVisible(isolate(f())), list(value=NULL, visible=TRUE))

  # invisible non-NULL value
  f <- reactive({ invisible(10)})
  expect_identical(withVisible(isolate(f())), list(value=10, visible=FALSE))
  expect_identical(withVisible(isolate(f())), list(value=10, visible=FALSE))

  # visible non-NULL value
  f <- reactive({ 10 })
  expect_identical(withVisible(isolate(f())), list(value=10, visible=TRUE))
  expect_identical(withVisible(isolate(f())), list(value=10, visible=TRUE))
})
