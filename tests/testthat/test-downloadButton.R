test_that("downloadButton starts disabled with correct attributes", {
  btn <- downloadButton("dl", "Download")
  html <- as.character(btn)

  expect_match(html, "class=.*disabled")
  expect_match(html, 'aria-disabled="true"')
  expect_match(html, 'tabindex="-1"')
  expect_match(html, 'href=""')
})

test_that("downloadLink starts disabled with correct attributes", {
  lnk <- downloadLink("dl", "Download")
  html <- as.character(lnk)

  expect_match(html, "class=.*disabled")
  expect_match(html, 'aria-disabled="true"')
  expect_match(html, 'tabindex="-1"')
  expect_match(html, 'href=""')
})

test_that("downloadButton omits data-ignore-update by default (enabled = 'auto')", {
  btn <- downloadButton("dl", "Download")
  html <- as.character(btn)

  expect_no_match(html, "data-ignore-update")
})

test_that("downloadButton has data-ignore-update when enabled = FALSE", {
  btn <- downloadButton("dl", "Download", enabled = FALSE)
  html <- as.character(btn)

  expect_match(html, "data-ignore-update")
})

test_that("downloadButton starts enabled when enabled = TRUE", {
  btn <- downloadButton("dl", "Download", enabled = TRUE)
  html <- as.character(btn)

  expect_no_match(html, "disabled")
  expect_no_match(html, "aria-disabled")
  expect_no_match(html, "tabindex")
  expect_no_match(html, "data-ignore-update")
})

test_that("downloadLink omits data-ignore-update by default (enabled = 'auto')", {
  lnk <- downloadLink("dl", "Download")
  html <- as.character(lnk)

  expect_no_match(html, "data-ignore-update")
})

test_that("downloadLink has data-ignore-update when enabled = FALSE", {
  lnk <- downloadLink("dl", "Download", enabled = FALSE)
  html <- as.character(lnk)

  expect_match(html, "data-ignore-update")
})

test_that("downloadLink starts enabled when enabled = TRUE", {
  lnk <- downloadLink("dl", "Download", enabled = TRUE)
  html <- as.character(lnk)

  expect_no_match(html, "disabled")
  expect_no_match(html, "aria-disabled")
  expect_no_match(html, "tabindex")
  expect_no_match(html, "data-ignore-update")
})

test_that("downloadButton snapshot (enabled = 'auto')", {
  expect_snapshot(downloadButton("dl", "Download"))
})

test_that("downloadButton snapshot (enabled = FALSE)", {
  expect_snapshot(downloadButton("dl", "Download", enabled = FALSE))
})

test_that("downloadButton snapshot (enabled = TRUE)", {
  expect_snapshot(downloadButton("dl", "Download", enabled = TRUE))
})

test_that("downloadLink snapshot (enabled = 'auto')", {
  expect_snapshot(downloadLink("dl", "Download"))
})

test_that("downloadLink snapshot (enabled = FALSE)", {
  expect_snapshot(downloadLink("dl", "Download", enabled = FALSE))
})

test_that("downloadLink snapshot (enabled = TRUE)", {
  expect_snapshot(downloadLink("dl", "Download", enabled = TRUE))
})
