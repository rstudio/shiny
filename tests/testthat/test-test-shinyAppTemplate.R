# testthat::skip_on_cran()
suppressWarnings(testthat::skip_if_not_installed("shinytest2"))
testthat::skip("Refactor test for next release")

# test all combos
make_combos <- function(...) {
  args <- list(...)
  combo_dt <- do.call(expand.grid, args)
  lapply(apply(combo_dt, 1, unlist), unname)
}

combos <- unique(unlist(
  recursive = FALSE,
  list(
    "all",
    # expand.grid on all combos
    make_combos("app", list(NULL, "module"), list(NULL, "rdir"), list(NULL, "tests")),
    # Legacy values
    list(list("app", "shinytest", "testthat"))
  )
))

for (combo in combos) {
  combo_name <- paste0(combo, collapse = "_")
  test_that(paste0("app template works with runTests: ", combo_name), {
    random_folder <- paste0("shinyAppTemplate-", combo_name)
    tempTemplateDir <- file.path(tempfile(), random_folder)
    suppressMessages(shinyAppTemplate(tempTemplateDir, combo))
    on.exit(unlink(tempTemplateDir, recursive = TRUE), add = TRUE)

    if (any(c("all", "tests", "shinytest", "testthat") %in% combo)) {
      suppressMessages(capture.output({
        out <- runTests(tempTemplateDir, assert = FALSE)
      }))
      expect_snapshot(out)
    } else {
      expect_error(
        suppressMessages(runTests(tempTemplateDir))
      )
    }
  })
}
