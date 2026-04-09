test_that("downloadButton starts disabled with correct attributes", {
  btn <- downloadButton("dl", "Download")
  html <- as.character(btn)

  expect_match(html, "class=.*disabled")
  expect_match(html, 'aria-disabled="true"')
  expect_match(html, 'tabindex="-1"')
  expect_match(html, 'href=""')
})

test_that("downloadButton omits data-ignore-update by default (autoEnable = TRUE)", {
  btn <- downloadButton("dl", "Download")
  html <- as.character(btn)

  expect_no_match(html, "data-ignore-update")
})

test_that("downloadButton has data-ignore-update when autoEnable = FALSE", {
  btn <- downloadButton("dl", "Download", autoEnable = FALSE)
  html <- as.character(btn)

  expect_match(html, "data-ignore-update")
})

test_that("downloadLink omits data-ignore-update by default (autoEnable = TRUE)", {
  lnk <- downloadLink("dl", "Download")
  html <- as.character(lnk)

  expect_no_match(html, "data-ignore-update")
})

test_that("downloadLink has data-ignore-update when autoEnable = FALSE", {
  lnk <- downloadLink("dl", "Download", autoEnable = FALSE)
  html <- as.character(lnk)

  expect_match(html, "data-ignore-update")
})

test_that("downloadButton snapshot (autoEnable = TRUE)", {
  expect_snapshot(downloadButton("dl", "Download"))
})

test_that("downloadButton snapshot (autoEnable = FALSE)", {
  expect_snapshot(downloadButton("dl", "Download", autoEnable = FALSE))
})

test_that("downloadLink snapshot (autoEnable = TRUE)", {
  expect_snapshot(downloadLink("dl", "Download"))
})

test_that("downloadLink snapshot (autoEnable = FALSE)", {
  expect_snapshot(downloadLink("dl", "Download", autoEnable = FALSE))
})
