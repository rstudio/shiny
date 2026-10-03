test_that("resume resolves the app's call, then SHINY_RESUME, then off", {
  withr::defer(shinyOptions(resume = NULL))
  withr::local_envvar(SHINY_RESUME = NA, SHINY_PORT = NA)
  expect_false(resume_settings()$enabled)                       # no local default-on
  withr::with_envvar(c(SHINY_RESUME = "TRUE"), expect_true(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_RESUME = "false"), expect_false(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_RESUME = "junk"), expect_false(resume_settings()$enabled))
  expect_identical(resume_settings()[names(resume_defaults())], resume_defaults())
  enableResume()
  withr::with_envvar(c(SHINY_RESUME = "FALSE"), expect_true(resume_settings()$enabled))
  disableResume()
  withr::with_envvar(c(SHINY_RESUME = "TRUE"), expect_false(resume_settings()$enabled))
})

test_that("enableResume() stores its settings and validates each argument by name", {
  withr::defer(shinyOptions(resume = NULL))
  expect_error(enableResume(reload = "maybe"), "'arg' should be one of")
  expect_error(enableResume(maxSize = -1), "`maxSize` must be a single positive number")
  expect_error(enableResume(maxNodeSize = "big"), "`maxNodeSize` must be a single positive number")
  expect_error(enableResume(debounce = c(1, 2)), "`debounce` must be a single non-negative number")
  expect_error(enableResume(appVersion = list("v1")), "`appVersion` must be NULL or a single string or number.", fixed = TRUE)
  expect_null(getShinyOption("resume"))           # a rejected call changes nothing
  expect_invisible(enableResume(reload = "fresh", appVersion = 2, maxSize = 1e6, maxNodeSize = 1e3, debounce = 0))
  expect_identical(
    getShinyOption("resume"),
    list(enabled = TRUE, reload = "fresh", appVersion = "2", maxSize = 1e6, maxNodeSize = 1e3, debounce = 0)
  )
  enableResume()
  expect_identical(getShinyOption("resume")$reload, "ask")
  expect_invisible(disableResume())
  expect_identical(getShinyOption("resume"), c(list(enabled = FALSE), resume_defaults()))
})

test_that("enableResume() and disableResume() refuse to run inside a session", {
  withr::defer(shinyOptions(resume = NULL))
  s <- MockShinySession$new()
  expect_error(withReactiveDomain(s, enableResume()),
    "Call enableResume() at the top level of the app (in app.R before shinyApp(), or in global.R), not inside a session.", fixed = TRUE)
  expect_error(withReactiveDomain(s, disableResume()),
    "Call disableResume() at the top level of the app to turn resume off, or pass it a reactive.", fixed = TRUE)
  expect_null(getShinyOption("resume"))
})

test_that("enableResume() and disableResume() refuse to run from server.R", {
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  for (fn in c("enableResume", "disableResume")) {
    dir <- withr::local_tempdir()
    writeLines("fluidPage()", file.path(dir, "ui.R"))
    writeLines(c(paste0(fn, "()"), "function(input, output) NULL"), file.path(dir, "server.R"))
    app <- shinyAppDir(dir)
    expect_error(app$serverFuncSource(),
      paste0("Call ", fn, "() in global.R, not server.R: server.R runs only once the first session has started."), fixed = TRUE)
  }
  expect_null(getShinyOption("resume"))
})

test_that("shinyApp() captures the resume setting for its own app and clears it", {
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  enableResume(maxSize = 1e6)
  a <- shinyApp(fluidPage(), function(input, output) NULL)
  expect_null(getShinyOption("resume"))
  disableResume()
  b <- shinyApp(fluidPage(), function(input, output) NULL)
  c3 <- shinyApp(fluidPage(), function(input, output) NULL)
  expect_true(a$appOptions$resume$enabled)
  expect_identical(a$appOptions$resume$maxSize, 1e6)
  expect_false(b$appOptions$resume$enabled)
  expect_null(c3$appOptions$resume)
})

test_that("disableResume(x) rejects objects it has no method for", {
  expect_error(disableResume(42),
    "disableResume() expects a reactiveVal, reactiveValues object, or observer, not an object of class numeric.", fixed = TRUE)
})
