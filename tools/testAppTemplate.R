# Run the tests that each shinyAppTemplate() combo writes, including its
# shinytest2 browser tests. These need Chrome and are slow, so they run once in
# the `app-template` job of .github/workflows/R-CMD-check.yaml instead of in
# tests/testthat/ on every OS and R version.
#
# Run from the repo root with shiny installed:
#   Rscript tools/testAppTemplate.R

library(shiny)

# Chrome can take more than chromote's default 10 seconds to start on a cold
# CI runner
options(chromote.timeout = 60)

# Every `examples` combo that includes tests
combos <- list(
  "all",
  c("app", "tests"),
  c("app", "module", "tests"),
  c("app", "rdir", "tests"),
  c("app", "module", "rdir", "tests"),
  # Legacy values
  c("app", "shinytest", "testthat")
)

# Returns the test files that failed
run_template_tests <- function(dir, ci) {
  # Snapshot expectations skip unless NOT_CRAN is set
  withr::local_envvar(CI = ci, NOT_CRAN = "true")
  res <- NULL
  utils::capture.output(res <- suppressMessages(runTests(dir, assert = FALSE)))
  sub(".*/app/", "", res$file[!res$pass])
}

failures <- character()
for (combo in combos) {
  name <- paste(combo, collapse = "_")
  dir <- file.path(tempfile(), "app")
  suppressMessages(shinyAppTemplate(dir, combo))

  # A new app has no snapshots, and testthat fails new snapshots on CI. So the
  # first run writes them as if not on CI, and the second run, on CI, must
  # match them.
  failed <- c(
    run_template_tests(dir, ci = "false"),
    run_template_tests(dir, ci = "true")
  )
  cat(name, if (length(failed)) paste("FAILED:", toString(unique(failed))) else "ok", "\n")
  if (length(failed)) failures <- c(failures, name)
}

if (length(failures)) {
  stop("App template tests failed for: ", toString(failures), call. = FALSE)
}
