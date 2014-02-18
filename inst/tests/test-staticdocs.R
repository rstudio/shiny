context("staticdocs")

test_that("All man pages have an entry in staticdocs/index.r", {
  if (!all(file.exists(c('../../staticdocs', '../../man')))) {
    # This test works only when run against a package directory
    return()
  }
  # Known not to be indexed
  known_unindexed <- c("shiny-package")

  indexed_topics <- local({
    result <- character(0)
    sd_section <- function(dummy1, dummy2, section_topics) {
      result <<- c(result, section_topics)
    }
    source("../../staticdocs/index.r", local = TRUE)
    result
  })

  all_topics <- sub("\\.Rd", "", list.files("../../man", pattern = "*.Rd"))

  # This test ensures that every documented topic is included in
  # staticdocs/index.r, unless explicitly waived by specifying it
  # in the known_unindexed variable above.
  expect_equivalent(sort(all_topics), sort(c(known_unindexed, indexed_topics)))
})
