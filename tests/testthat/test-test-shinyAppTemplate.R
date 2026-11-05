# Check the files each combo creates. The template's own tests need a browser
# and are slow, so they run in tools/testAppTemplate.R, in the `app-template`
# CI job.

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

template_files <- function(combo) {
  dir <- file.path(tempfile(), "app")
  withr::defer(unlink(dirname(dir), recursive = TRUE))
  suppressMessages(shinyAppTemplate(dir, combo))
  sort(list.files(dir, recursive = TRUE))
}

test_that("shinyAppTemplate() creates the expected files for each combo", {
  # shinyAppTemplate() requires shinytest2 for templates with tests
  suppressWarnings(skip_if_not_installed("shinytest2"))

  expect_snapshot({
    for (combo in combos) {
      cat(paste0(combo, collapse = "_"), ":\n")
      cat(paste0("  ", template_files(combo), "\n"), sep = "")
    }
  })
})

test_that("runTests() errors for a template without tests", {
  dir <- file.path(tempfile(), "app")
  withr::defer(unlink(dirname(dir), recursive = TRUE))
  suppressMessages(shinyAppTemplate(dir, "app"))
  expect_error(suppressMessages(runTests(dir)))
})
