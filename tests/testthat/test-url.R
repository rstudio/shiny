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

  # Leading/trailing/consecutive ampersands are ignored
  expect_identical(parseQueryString("?&a=1&&b=2&"), parseQueryString("?a=1&b=2"))

  # Nested and non-nested query strings
  expect_identical(
    parseQueryString("a[i1][j1]=x&b[i1][j1]=y&b[i2][j1]=z"),
    list(
      "a[i1][j1]" = "x",
      "b[i1][j1]" = "y",
      "b[i2][j1]" = "z"
    )
  )

  expect_identical(
    parseQueryString("a[i1][j1]=x&b[i1][j1]=y&b[i2][j1]=z", nested = TRUE),
    list(
      a = list(i1 = list(j1 = "x")),
      b = list(
        i1 = list(j1 = "y"),
        i2 = list(j1 = "z")
      )
    )
  )
})
