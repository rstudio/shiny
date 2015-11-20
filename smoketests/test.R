source("functions.R")

options(warn = 1)

failures <- 0
for (dir in appdirs()) {
  cat("Testing", dir, "\n")

  snapshotPath <- file.path(dir, "R.out.save")
  if (!upToDate(dir, "R.out.save")) {
    warning(dir, " snapshot may be out of date")
  }

  res <- executeApp(dir)
  if (!identical(readLines(snapshotPath), res)) {
    resultPath <- file.path("output", dir, "R.out")
    dir.create(dirname(resultPath), showWarnings = FALSE, recursive = TRUE, mode = "0775")
    writeLines(res, resultPath)
    message("Results differ! Writing output to ", resultPath)
    failures <- failures + 1
  }
}

if (failures) {
  cat(file = stderr(), paste0("--\n", failures, " test(s) failed\n"))
  q("no", status = 1)
}
