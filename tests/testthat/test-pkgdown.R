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

  can_test <- any(
    # called using devtools::test()
    identical(Sys.getenv("NOT_CRAN", FALSE), "true"),
    # called within CI testing
    isTRUE(as.logical(Sys.getenv("CI")))
  )
  if (!can_test) {
    skip("Not called using `devtools::test()` (`NOT_CRAN=true`) or in CI (`CI=true`)")
  }

  indexed_topics <- get_indexed()
  all_topics     <- get_exported()

  ## Known not to be indexed
  reexports_man_info <- jsonlite::fromJSON(system.file("_reexports.json", package = "shiny"), simplifyDataFrame = FALSE)
  reexports_man_file_names <- unlist(
    recursive = TRUE,
    lapply(reexports_man_info, function(alias_pkg_info) {
      lapply(alias_pkg_info$exports, function(man_item) {
        sub(".Rd", "", man_item$file, fixed = TRUE)
      })
    })
  )
  known_unindexed <- c("shiny-package", "stacktrace", "knitr_methods",
                       "pageWithSidebar", "headerPanel", "shiny.appobj",
                       "deprecatedReactives", "reexports")

  ## This test ensures that every documented topic is included in
  ## staticdocs/index.r, unless explicitly waived by specifying it
  ## in the known_unindexed variable above.
  missing <- setdiff(all_topics, c(known_unindexed, indexed_topics))
  ## Explicitly add reexports man files as they will be added at shiny-dev-center documentation build time
  unknown <- setdiff(c(known_unindexed, indexed_topics), c(all_topics, reexports_man_file_names))

  expect_equal(length(missing), 0,
    info = paste("Functions missing from _pkgdown.yml:\n",
      paste("  ", missing, sep = "", collapse = "\n"),
      sep = ""))
  expect_equal(length(unknown), 0,
    info = paste("Unrecognized functions in _pkgdown.yml:\n",
      paste("  ", unknown, sep = "", collapse = "\n"),
      sep = ""))
})
