context("Validate Compiled shiny.js File")

test_that("{{ VERSION }} was replaced", {
  jsFiles <- system.file(
    file.path("www", "shared", c("shiny.js", "shiny.min.js")),
    package = "shiny"
  )

  lapply(jsFiles, function(jsFile) {
    jsFileContent <- paste(suppressWarnings(readLines(jsFile)), collapse = "\n")
    expect_false(grepl("\\{\\{\\sVERSION\\s\\}\\}", jsFileContent))
  })
})
