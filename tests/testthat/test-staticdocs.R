context("staticdocs")

test_that("All man pages have an entry in staticdocs/index.r", {
  if (all(file.exists(c('../../inst/staticdocs', '../../man')))) {
    # We're running tests on a source tree
    mode <- "source"
  } else if (all(file.exists(c('../../shiny/staticdocs', '../../shiny/html')))) {
    # We're testing an installed package, possibly for R CMD check
    mode <- "bundle"
  } else {
    cat("Unknown testing environment for test-staticdocs.R.\n", file = stderr())
    return()
  }

  # Known not to be indexed
  known_unindexed <- c("shiny-package", "stacktrace", "knitr_methods", "knitr_methods_htmltools")

  # Read in topics from a staticdocs/index.r file
  get_indexed_topics <- function(index_path) {
    result <- character(0)
    sd_section <- function(dummy1, dummy2, section_topics) {
      result <<- c(result, section_topics)
    }
    source(index_path, local = TRUE)
    result
  }

  if (mode == "source") {
    indexed_topics <- get_indexed_topics("../../inst/staticdocs/index.r")
    all_topics <- sub("\\.Rd", "", list.files("../../man", pattern = "*.Rd"))

  } else if (mode == "bundle") {
    indexed_topics <- get_indexed_topics("../../shiny/staticdocs/index.r")
    all_topics <- unique(unname(readRDS("../../shiny/help/aliases.rds")))
  }

  # This test ensures that every documented topic is included in
  # staticdocs/index.r, unless explicitly waived by specifying it
  # in the known_unindexed variable above.
  missing <- setdiff(sort(all_topics), sort(c(known_unindexed, indexed_topics)))
  unknown <- setdiff(sort(c(known_unindexed, indexed_topics)), sort(all_topics))
  expect_equal(length(missing), 0,
    info = paste("Functions missing from index:\n",
      paste("  ", missing, sep = "", collapse = "\n"),
      sep = ""))
  expect_equal(length(unknown), 0,
    info = paste("Unrecognized functions in index.r:\n",
      paste("  ", unknown, sep = "", collapse = "\n"),
      sep = ""))
})
