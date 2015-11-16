source("functions.R")

for (dir in appdirs()) {
  snapshotPath <- file.path(dir, "R.out.save")
  if (upToDate(dir, "R.out.save"))
    next

  cat("Snapshotting", dir, "\n")
  res <- executeApp(dir)
  writeLines(res, snapshotPath)
}

invisible()
