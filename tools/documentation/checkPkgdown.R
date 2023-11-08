local({

  reexports_file <- rprojroot::find_package_root_file("tools/documentation/reexports.json")
  pkgdown_file <- rprojroot::find_package_root_file("tools/documentation/pkgdown.yml")

  get_exported <- function() {
    # We're running tests on a source tree, likely by devtools::test()
    sub("\\.Rd", "", list.files(rprojroot::find_package_root_file("man"), pattern = "*.Rd"))

    # # We're testing an installed package, possibly for R CMD check
    # unique(unname(readRDS("../../shiny/help/aliases.rds")))
  }

  get_indexed <- function(pkgdown_file) {
    unlist(lapply(yaml::yaml.load_file(pkgdown_file)$reference, function(x) x$contents))
  }

  indexed_topics <- get_indexed(pkgdown_file)
  all_topics     <- get_exported()

  ## Known not to be indexed
  reexports_man_info <- jsonlite::fromJSON(reexports_file, simplifyDataFrame = FALSE)
  reexports_man_file_names <- unlist(
    recursive = TRUE,
    lapply(reexports_man_info, function(alias_pkg_info) {
      lapply(alias_pkg_info$exports, function(man_item) {
        sub(".Rd", "", man_item$file, fixed = TRUE)
      })
    })
  )

  ## This test ensures that every documented topic is included in
  ## staticdocs/index.r, unless explicitly waived by specifying it
  ## as `@keywords internal`
  no_entry_topics <- setdiff(all_topics, indexed_topics)
  internal_topics <- unlist(lapply(
    all_topics,
    function(topic) {
      topic_txt <- readLines(rprojroot::find_package_root_file(paste0("man/", topic, ".Rd")))
      if (
        any(grepl("\\keyword{internal}", topic_txt, fixed = TRUE)) ||
        any(grepl("\\docType{package}", topic_txt, fixed = TRUE))
      ) {
        # Return internal topic name
        topic
      } else {
        NULL
      }
    }
  ))

  # # Make sure internal functions to NOT have an entry
  # displayed_internal_topics <- internal_topics[internal_topics %in% indexed_topics]
  # testthat::expect_equal(length(displayed_internal_topics), 0,
  #   info = paste("Functions listed in ./tools/documentation/pkgdown.yml but has the keyword internal:\n",
  #     paste(" - ", displayed_internal_topics, sep = "", collapse = "\n"),
  #     "\nPlease update ./tools/documentation/pkgdown.yml or make it `#' @keywords internal`",
  #     sep = ""))

  # Make sure there are no non-internal topics have an entry
  missing_entry_topics <- setdiff(no_entry_topics, internal_topics)
  testthat::expect_equal(length(missing_entry_topics), 0,
    info = paste("Functions missing from ./tools/documentation/pkgdown.yml:\n",
      paste(" - ", missing_entry_topics, sep = "", collapse = "\n"),
      "\nPlease update ./tools/documentation/pkgdown.yml or remove `#' @keywords internal`",
      sep = ""))

  ## Explicitly add reexports man files as they will be added at shiny-dev-center documentation build time
  unknown_topics <- setdiff(indexed_topics, c(all_topics, reexports_man_file_names))
  testthat::expect_equal(length(unknown_topics), 0,
    info = paste("Unrecognized functions in ./tools/documentation/pkgdown.yml:\n",
      paste(" - ", unknown_topics, sep = "", collapse = "\n"),
      "\nPlease update ./tools/documentation/pkgdown.yml",
      sep = ""))
  invisible(TRUE)
})
