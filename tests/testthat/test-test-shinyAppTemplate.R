# Running the template's own tests would need a browser, and its shinytest2
# tests only ever write new snapshots (each run uses a new temp app), which
# fail on CI. So check the files each combo creates instead.

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
