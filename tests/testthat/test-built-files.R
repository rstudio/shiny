test_that("shiny.js version was replaced", {
  jsFiles <- system.file(
    file.path("www", "shared", c("shiny.js", "shiny.min.js")),
    package = "shiny"
  )

  lapply(jsFiles, function(jsFile) {
    jsFileContent <- paste(suppressWarnings(readLines(jsFile)), collapse = "\n")
    expect_false(grepl("\\{\\{\\sVERSION\\s\\}\\}", jsFileContent))
  })
})

test_that("shiny.css has been built", {
  skip_on_cran()
  skip_if_not_installed("rprojroot")
  # These tests can only be run when we have access to the source
  # (i.e., they are skipped with `R CMD check`)
  build_script <- "../../tools/updateShinyCSS.R"
  if (file.exists(build_script)) {
    target <- rprojroot::find_package_root_file("inst/www/shared/shiny.min.css")
    expect_true(file.exists(target))
    pre_build_time <- file.mtime(target)
    pre_build_content <- readLines(target)
    source(build_script, local = TRUE)
    expect_true(file.mtime(target) > pre_build_time)
    expect_identical(readLines(target), pre_build_content)
  } else {
    skip("Not able to verify whether shiny.css has been built")
  }
})
