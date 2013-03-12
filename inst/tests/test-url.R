context("URL")

test_that("Query string parsing", {
  expect_identical(
    parseQueryString("?foo=1&bar=b+a%20r&b+a%20z=baz&=nokey&novalue=&=&noequal&end=end"),
    list(
      foo     = '1',
      bar     = 'b a r',
      `b a z` = 'baz',
                'nokey',
      novalue = '',
                '',
      noequal = '',
      end     = 'end'
    )
  )

  # Should be the same with or without leading question mark
  expect_identical(parseQueryString("?foo=1&bar=b"), parseQueryString("foo=1&bar=b"))
})
