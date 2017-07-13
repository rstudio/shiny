context("apps")

if (dir.exists("apps")) {
  app_dirs <- Filter(dir.exists, dir("apps", full.names = TRUE))
  if (length(app_dirs) > 0) {
    for (app_dir in app_dirs) {
      test_that(basename(app_dir), {
        shinytest::expect_pass(shinytest::testApp(app_dir, compareScreenshot = FALSE))
      })
    }
  }
}
