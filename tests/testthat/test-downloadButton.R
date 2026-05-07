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

test_that("downloadButton omits data-shiny-disable-auto-enable by default (enabled = 'auto')", {
  btn <- downloadButton("dl", "Download")
  html <- as.character(btn)

  expect_no_match(html, "data-shiny-disable-auto-enable")
})

test_that("downloadButton has data-shiny-disable-auto-enable when enabled = FALSE", {
  btn <- downloadButton("dl", "Download", enabled = FALSE)
  html <- as.character(btn)

  expect_match(html, "data-shiny-disable-auto-enable")
})

test_that("downloadButton starts enabled when enabled = TRUE", {
  btn <- downloadButton("dl", "Download", enabled = TRUE)
  html <- as.character(btn)

  expect_no_match(html, "disabled")
  expect_no_match(html, "aria-disabled")
  expect_no_match(html, "tabindex")
  expect_match(html, "data-shiny-disable-auto-enable")
})

test_that("downloadLink omits data-shiny-disable-auto-enable by default (enabled = 'auto')", {
  lnk <- downloadLink("dl", "Download")
  html <- as.character(lnk)

  expect_no_match(html, "data-shiny-disable-auto-enable")
})

test_that("downloadLink has data-shiny-disable-auto-enable when enabled = FALSE", {
  lnk <- downloadLink("dl", "Download", enabled = FALSE)
  html <- as.character(lnk)

  expect_match(html, "data-shiny-disable-auto-enable")
})

test_that("downloadLink starts enabled when enabled = TRUE", {
  lnk <- downloadLink("dl", "Download", enabled = TRUE)
  html <- as.character(lnk)

  expect_no_match(html, "disabled")
  expect_no_match(html, "aria-disabled")
  expect_no_match(html, "tabindex")
  expect_match(html, "data-shiny-disable-auto-enable")
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

test_that("downloadButton errors on invalid enabled value", {
  expect_error(downloadButton("dl", "Download", enabled = 1), "enabled")
})

test_that("downloadButton errors on NA enabled value", {
  expect_error(downloadButton("dl", "Download", enabled = NA), "enabled")
})

test_that("downloadLink errors on invalid enabled value", {
  expect_error(downloadLink("dl", "Download", enabled = 1), "enabled")
})

test_that("downloadLink errors on NA enabled value", {
  expect_error(downloadLink("dl", "Download", enabled = NA), "enabled")
})
