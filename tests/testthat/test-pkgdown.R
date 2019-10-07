context("pkgdown")

get_exported <- function() {
  if (all(file.exists(c('../../inst/_pkgdown.yml', '../../man')))) {
    # We're running tests on a source tree, likely by devtools::test()
    sub("\\.Rd", "", list.files("../../man", pattern = "*.Rd"))
  } else {
    # We're testing an installed package, possibly for R CMD check
    unique(unname(readRDS("../../shiny/help/aliases.rds")))
  }
}

get_indexed <- function(f = system.file('_pkgdown.yml', package = 'shiny')) {
  unlist(lapply(yaml::yaml.load_file(f)$reference, function(x) x$contents))
}

test_that("All man pages have an entry in _pkgdown.yml", {
  skip_on_cran()
  indexed_topics <- get_indexed()
  all_topics     <- get_exported()

  ## Known not to be indexed
  known_unindexed <- c("shiny-package", "stacktrace", "knitr_methods",
                       "pageWithSidebar", "headerPanel", "shiny.appobj",
                       "deprecatedReactives")

  ## This test ensures that every documented topic is included in
  ## staticdocs/index.r, unless explicitly waived by specifying it
  ## in the known_unindexed variable above.
  missing <- setdiff(all_topics, c(known_unindexed, indexed_topics))
  unknown <- setdiff(c(known_unindexed, indexed_topics), all_topics)
  expect_equal(length(missing), 0,
    info = paste("Functions missing from _pkgdown.yml:\n",
      paste("  ", missing, sep = "", collapse = "\n"),
      sep = ""))
  expect_equal(length(unknown), 0,
    info = paste("Unrecognized functions in _pkgdown.yml:\n",
      paste("  ", unknown, sep = "", collapse = "\n"),
      sep = ""))
})
