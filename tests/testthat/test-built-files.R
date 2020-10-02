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
  new_css <- sass::sass(
    sass::sass_file(system.file("www/shared/shiny_scss/shiny.scss", package = "shiny")),
    cache = NULL,
    options = sass::sass_options(output_style = "compressed"),
  )
  # Remove class and attributes
  new_css <- as.character(new_css)

  pkg_css_file <- system.file("www/shared/shiny.min.css", package = "shiny")
  pkg_css <- readChar(pkg_css_file, file.size(pkg_css_file), useBytes = TRUE)

  # If this fails, that means that tools/updateShinyCSS.R needs to be run.
  expect_identical(new_css, pkg_css)
})
