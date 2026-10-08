test_that("resume resolves the app's call, then SHINY_RESUME, then off", {
  withr::defer(shinyOptions(resume = NULL))
  withr::local_envvar(SHINY_RESUME = NA, SHINY_PORT = NA)
  expect_false(resume_settings()$enabled)
  withr::with_envvar(c(SHINY_RESUME = "TRUE"), expect_true(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_RESUME = "false"), expect_false(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_RESUME = "junk"), expect_false(resume_settings()$enabled))
  expect_identical(resume_settings()[names(resume_defaults())], resume_defaults())
  enableResume()
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), expect_true(resume_settings()$enabled))
  enableResume(FALSE)
  withr::with_envvar(c(SHINY_RESUME = "TRUE"), expect_false(resume_settings()$enabled))
})

test_that("enableResume() stores its settings and validates each argument by name", {
  withr::defer(shinyOptions(resume = NULL))
  expect_error(enableResume(maxSize = -1), "`maxSize` must be a single positive number")
  expect_error(enableResume(maxNodeSize = "big"), "`maxNodeSize` must be a single positive number")
  expect_error(enableResume(debounce = c(1, 2)), "`debounce` must be a single non-negative number")
  expect_error(enableResume(appVersion = list("v1")), "`appVersion` must be NULL or a single string or number.", fixed = TRUE)
  expect_null(getShinyOption("resume"))
  expect_invisible(enableResume(appVersion = 2, maxSize = 1e6, maxNodeSize = 1e3, debounce = 0))
  expect_identical(
    getShinyOption("resume"),
    list(enabled = TRUE, appVersion = "2", maxSize = 1e6, maxNodeSize = 1e3, debounce = 0)
  )
  expect_invisible(enableResume(FALSE))
  expect_identical(getShinyOption("resume"), c(list(enabled = FALSE), resume_defaults()))
})

test_that("enableResume(FALSE) ignores the other arguments without validating them", {
  withr::defer(shinyOptions(resume = NULL))
  enableResume(FALSE, appVersion = list("v1"), maxSize = -1, maxNodeSize = "big", debounce = c(1, 2))
  expect_identical(getShinyOption("resume"), c(list(enabled = FALSE), resume_defaults()))
})

test_that("`enabled` must be TRUE or FALSE", {
  withr::defer(shinyOptions(resume = NULL))
  for (bad in list(NA, "yes", c(TRUE, FALSE), 1, NULL)) {
    expect_error(enableResume(bad), "`enabled` must be TRUE or FALSE.", fixed = TRUE)
  }
  expect_null(getShinyOption("resume"))
})

test_that("enableResume() has no reload argument, and disableResume() is gone", {
  expect_false("reload" %in% names(formals(enableResume)))
  expect_false("reload" %in% names(resume_defaults()))
  expect_false(exists("disableResume", envir = asNamespace("shiny"), inherits = FALSE))
})

test_that("enableResume() refuses to run inside a session, either way", {
  withr::defer(shinyOptions(resume = NULL))
  s <- MockShinySession$new()
  msg <- "Call enableResume() at the top level of the app (in app.R before shinyApp(), or in global.R), not inside a session."
  expect_error(withReactiveDomain(s, enableResume()), msg, fixed = TRUE)
  expect_error(withReactiveDomain(s, enableResume(FALSE)), msg, fixed = TRUE)
  expect_null(getShinyOption("resume"))
})

test_that("enableResume() refuses to run from server.R, either way", {
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  for (call in c("enableResume()", "enableResume(FALSE)")) {
    dir <- withr::local_tempdir()
    writeLines("fluidPage()", file.path(dir, "ui.R"))
    writeLines(c(call, "function(input, output) NULL"), file.path(dir, "server.R"))
    app <- shinyAppDir(dir)
    expect_error(app$serverFuncSource(),
      "Call enableResume() in global.R, not server.R: server.R runs only once the first session has started.", fixed = TRUE)
  }
  expect_null(getShinyOption("resume"))
})

test_that("shinyApp() captures the resume setting for its own app and clears it", {
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  enableResume(maxSize = 1e6)
  a <- shinyApp(fluidPage(), function(input, output) NULL)
  expect_null(getShinyOption("resume"))
  enableResume(FALSE)
  b <- shinyApp(fluidPage(), function(input, output) NULL)
  c3 <- shinyApp(fluidPage(), function(input, output) NULL)
  expect_true(a$appOptions$resume$enabled)
  expect_identical(a$appOptions$resume$maxSize, 1e6)
  expect_false(b$appOptions$resume$enabled)
  expect_null(c3$appOptions$resume)
})
