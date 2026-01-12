test_that("render functions in UI throw informative error", {
  expect_error(as.tags.shiny.render.function(renderTable("table")),
               "There is a renderXXX function in the UI")
})

test_that("isTemp passes sanity checks", {
  t <- tempfile(fileext = ".txt")
  writeLines("hello", t)
  on.exit(unlink(t), add = TRUE)

  expect_true(isTemp(t, mustExist = TRUE))
  expect_false(isTemp(path.expand("~"), mustExist = TRUE))
  expect_false(isTemp(".", mustExist = TRUE))

  # Tempdir itself isn't a temp file
  expect_false(isTemp(tempdir(), mustExist = TRUE))

  # Malformed temp dir isn't a problem
  expect_error(isTemp(path.expand("~"), tempDir = "", mustExist = TRUE))

  # path normalization
  expect_true(isTemp(".", tempDir = normalizePath(".."), mustExist = TRUE))
  expect_true(isTemp(normalizePath("."), tempDir = "..", mustExist = TRUE))
  expect_true(isTemp(".", tempDir = "..", mustExist = TRUE))

  # not based on simple string matching
  subdir <- tempfile()
  dir.create(subdir)
  on.exit(unlink(subdir), add = TRUE)

  badpath <- paste0(subdir, "1")
  writeLines("hello", badpath)
  on.exit(unlink(badpath), add = TRUE)

  expect_false(isTemp(badpath, tempDir = subdir, mustExist = TRUE))
  # While we're here, make sure that paths can count as temp
  expect_true(isTemp(subdir, mustExist = TRUE))
})

test_that("isTemp symlink scenarios", {
  testthat::skip_on_os("windows")

  faketmp <- file.path(getwd(), "faketmp")
  file.symlink(tempdir(), faketmp)
  on.exit(unlink(faketmp), add = TRUE)

  t <- tempfile(fileext = ".txt")
  writeLines("hello", t)
  on.exit(unlink(t), add = TRUE)

  expect_true(isTemp(t, mustExist = TRUE))
  expect_true(isTemp(t, tempDir = faketmp, mustExist = TRUE))
  expect_true(isTemp(file.path(faketmp, basename(t)), mustExist = TRUE))
  expect_true(isTemp(file.path(faketmp, basename(t)), tempDir = faketmp, mustExist = TRUE))
})
