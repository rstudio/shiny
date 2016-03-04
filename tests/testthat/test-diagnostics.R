context("code diagnostics")

test_that("Code diagnostics", {
  suppressMessages({
    expect_false(diagnoseCode(text = "div(,)"))
    expect_false(diagnoseCode(text = "div(a,)"))
    expect_false(diagnoseCode(text = "div(,a)"))
    expect_false(diagnoseCode(text = "div(a,,b)"))
    expect_false(diagnoseCode(text = "div(a,\n,b)"))
    expect_false(diagnoseCode(text = "div(a,,b,)"))
    expect_false(diagnoseCode(text = "div(a,b))"))
    expect_false(diagnoseCode(text = "div()}"))
    expect_false(diagnoseCode(text = "div())"))
    expect_false(diagnoseCode(text = "div()]"))
  })


  # Ambiguous - these aren't valid R code, but they're outside the scope of
  # diagnoseCode, at least for now.
  # expect_false(diagnoseCode(text = "a,,b"))
  # expect_false(diagnoseCode(text = "div(a, ==2 )"))
  # expect_false(diagnoseCode(text = "div(a,!,b)"))
  # expect_false(diagnoseCode(text = "1 2"))

  # Should not error
  expect_true(diagnoseCode(text = "div()"))
  expect_true(diagnoseCode(text = "div(a)"))
  expect_true(diagnoseCode(text = "div(a,b)"))
  expect_true(diagnoseCode(text = "div(1,'b')"))
  expect_true(diagnoseCode(text = "div(a,~b)"))
  expect_true(diagnoseCode(text = "div([mtcars,,FALSE])"))
  expect_true(diagnoseCode(text = "div(a, 1==2)"))

  # Outside of () scope
  expect_true(diagnoseCode(text = "1\n2"))
})
