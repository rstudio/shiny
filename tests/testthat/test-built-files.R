test_that("shiny.js was built for this package version", {
  jsFiles <- system.file(
    file.path("www", "shared", c("shiny.js", "shiny.min.js")),
    package = "shiny"
  )
  banner <- paste0("/*! shiny ", packageVersion("shiny"), " |")

  for (jsFile in jsFiles) {
    expect_true(startsWith(readLines(jsFile, n = 1), banner), label = jsFile)
  }
})
