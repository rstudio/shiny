context("text")

test_that("reactivePrint and reactiveText behavior is correct", {
  expect_equal(isolate(reactivePrint(function() "foo")()),
               '[1] "foo"')
  expect_equal(isolate(reactivePrint(function() invisible("foo"))()),
               '')
  expect_equal(isolate(reactivePrint(function() { print("foo"); "bar"})()),
               '[1] "foo"\n[1] "bar"')
  expect_equal(isolate(reactivePrint(function() NULL)()),
               'NULL')
  expect_equal(isolate(reactivePrint(function() invisible())()),
               '')
  expect_equal(isolate(reactivePrint(function() 1:5)()),
               '[1] 1 2 3 4 5')
  
  expect_equal(isolate(reactiveText(function() "foo")()),
               'foo')
  expect_equal(isolate(reactiveText(function() invisible("foo"))()),
               'foo')
  expect_equal(isolate(reactiveText(function() { print("foo"); "bar"})()),
               'bar')
  expect_equal(isolate(reactiveText(function() NULL)()),
               '')
  expect_equal(isolate(reactiveText(function() invisible())()),
               '')
  expect_equal(isolate(reactiveText(function() 1:5)()),
               '1 2 3 4 5')  
})
