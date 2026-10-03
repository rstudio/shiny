upload_value <- function(dir, ...) {
  contents <- c(...)
  paths <- vapply(seq_along(contents), function(i) {
    # Unique per call: two inputs must not share a source file.
    p <- tempfile(sprintf("u%d-", i), tmpdir = dir, fileext = ".csv"); writeLines(contents[[i]], p); p
  }, character(1))
  data.frame(name = basename(paths), size = file.size(paths), type = "text/csv",
             datapath = paths, stringsAsFactors = FALSE)
}

test_that("the plan keeps uploads in input order within the budget and omits the rest", {
  dir <- withr::local_tempdir()
  inputs <- list(a = upload_value(dir, "aaaa"), b = upload_value(dir, "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"), c = upload_value(dir, "cc"))
  plan <- snapshot_upload_plan(inputs, c("a", "b", "c"), budget = 12)
  expect_identical(names(plan$keep), c("a", "c"))
  expect_identical(plan$omitted, "b")
  expect_match(plan$keep$a, "^a-1-[0-9a-f]{8}\\.csv$")
})

test_that("uploads are copied once beside the record, datapaths rewritten, stale copies removed", {
  dir <- withr::local_tempdir()
  files <- file.path(dir, "k-files")
  rec <- list(inputs = list(a = upload_value(dir, "first")), uploads = NULL)
  rec$uploads <- snapshot_upload_plan(rec$inputs, "a", Inf)$keep
  out <- snapshot_write_uploads(rec, files)
  expect_identical(out$inputs$a$datapath, rec$uploads$a)
  expect_identical(readLines(file.path(files, out$inputs$a$datapath)), "first")
  mtime <- file.mtime(file.path(files, out$inputs$a$datapath))
  Sys.sleep(1.1)
  snapshot_write_uploads(rec, files)
  expect_identical(file.mtime(file.path(files, out$inputs$a$datapath)), mtime)   # not copied again
  rec2 <- list(inputs = list(a = upload_value(dir, "second")))
  rec2$uploads <- snapshot_upload_plan(rec2$inputs, "a", Inf)$keep
  snapshot_write_uploads(rec2, files)
  expect_length(list.files(files), 1)
  expect_false(file.exists(file.path(files, out$inputs$a$datapath)))
})

test_that("restoring copies each upload out of the record's files; a missing file restores as NULL", {
  dir <- withr::local_tempdir()
  files <- file.path(dir, "k-files")
  rec <- list(inputs = list(a = upload_value(dir, "x", "y")), fileInputs = "a")
  rec$uploads <- snapshot_upload_plan(rec$inputs, "a", Inf)$keep
  rec <- snapshot_write_uploads(rec, files)
  s <- MockShinySession$new()
  s$restoreContext <- local({ ctx <- RestoreContext$new(); ctx$set(active = FALSE, dir = files); ctx })
  restored <- snapshot_restore_uploads(rec, s)
  expect_identical(nrow(restored$inputs$a), 2L)
  expect_true(all(file.exists(restored$inputs$a$datapath)))
  expect_false(any(startsWith(restored$inputs$a$datapath, files)))   # a copy, not the record's file
  unlink(files, recursive = TRUE)
  expect_null(snapshot_restore_uploads(rec, s)$inputs$a)
})
