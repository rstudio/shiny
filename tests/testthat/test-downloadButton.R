test_that("downloadButton starts disabled with correct attributes", {
  btn <- downloadButton("dl", "Download")

  expect_equal(htmltools::tagGetAttribute(btn, "class"), "btn btn-default shiny-download-link disabled")
  expect_equal(htmltools::tagGetAttribute(btn, "aria-disabled"), "true")
  expect_equal(htmltools::tagGetAttribute(btn, "tabindex"), "-1")
  expect_equal(htmltools::tagGetAttribute(btn, "href"), "")
})

test_that("downloadLink starts disabled with correct attributes", {
  lnk <- downloadLink("dl", "Download")

  expect_equal(htmltools::tagGetAttribute(lnk, "class"), "shiny-download-link disabled")
  expect_equal(htmltools::tagGetAttribute(lnk, "aria-disabled"), "true")
  expect_equal(htmltools::tagGetAttribute(lnk, "tabindex"), "-1")
  expect_equal(htmltools::tagGetAttribute(lnk, "href"), "")
})

test_that("downloadButton omits data-shiny-disable-auto-enable by default (enabled = 'auto')", {
  btn <- downloadButton("dl", "Download")

  expect_null(htmltools::tagGetAttribute(btn, "data-shiny-disable-auto-enable"))
})

test_that("downloadButton has data-shiny-disable-auto-enable when enabled = FALSE", {
  btn <- downloadButton("dl", "Download", enabled = FALSE)

  expect_true(!is.null(htmltools::tagGetAttribute(btn, "data-shiny-disable-auto-enable")))
})

test_that("downloadButton starts enabled when enabled = TRUE", {
  btn <- downloadButton("dl", "Download", enabled = TRUE)

  expect_null(htmltools::tagGetAttribute(btn, "aria-disabled"))
  expect_null(htmltools::tagGetAttribute(btn, "tabindex"))
  expect_true(!is.null(htmltools::tagGetAttribute(btn, "data-shiny-disable-auto-enable")))
  expect_false(grepl("disabled", htmltools::tagGetAttribute(btn, "class")))
})

test_that("downloadLink omits data-shiny-disable-auto-enable by default (enabled = 'auto')", {
  lnk <- downloadLink("dl", "Download")

  expect_null(htmltools::tagGetAttribute(lnk, "data-shiny-disable-auto-enable"))
})

test_that("downloadLink has data-shiny-disable-auto-enable when enabled = FALSE", {
  lnk <- downloadLink("dl", "Download", enabled = FALSE)

  expect_true(!is.null(htmltools::tagGetAttribute(lnk, "data-shiny-disable-auto-enable")))
})

test_that("downloadLink starts enabled when enabled = TRUE", {
  lnk <- downloadLink("dl", "Download", enabled = TRUE)

  expect_null(htmltools::tagGetAttribute(lnk, "aria-disabled"))
  expect_null(htmltools::tagGetAttribute(lnk, "tabindex"))
  expect_true(!is.null(htmltools::tagGetAttribute(lnk, "data-shiny-disable-auto-enable")))
  expect_false(grepl("disabled", htmltools::tagGetAttribute(lnk, "class")))
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
