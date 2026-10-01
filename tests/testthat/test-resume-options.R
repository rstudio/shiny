test_that("resume resolves the app's call, then SHINY_RESUME, then the hosted heuristic", {
  withr::defer(shinyOptions(resume = NULL))
  withr::local_envvar(SHINY_RESUME = NA, SHINY_PORT = NA)
  expect_true(resume_settings()$enabled)
  withr::with_envvar(c(SHINY_PORT = "1234"), expect_false(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_PORT = "1234", SHINY_RESUME = "TRUE"), expect_true(resume_settings()$enabled))
  withr::with_envvar(c(SHINY_RESUME = "false"), expect_false(resume_settings()$enabled))
  expect_identical(resume_settings()[names(resume_defaults())], resume_defaults())
  disableResume()
  withr::with_envvar(c(SHINY_RESUME = "TRUE"), expect_false(resume_settings()$enabled))
  enableResume()
  withr::with_envvar(c(SHINY_PORT = "1234", SHINY_RESUME = "FALSE"), expect_true(resume_settings()$enabled))
})

test_that("enableResume() stores its settings and validates each argument by name", {
  withr::defer(shinyOptions(resume = NULL))
  expect_error(enableResume(maxSize = -1), "`maxSize` must be a single positive number")
  expect_error(enableResume(maxNodeSize = "big"), "`maxNodeSize` must be a single positive number")
  expect_error(enableResume(debounce = c(1, 2)), "`debounce` must be a single non-negative number")
  expect_error(enableResume(appVersion = list("v1")), "`appVersion` must be NULL or a single string or number.", fixed = TRUE)
  expect_error(enableResume(appVersion = NA_character_), "`appVersion` must be NULL or a single string or number.", fixed = TRUE)
  expect_null(getShinyOption("resume"))           # a rejected call changes nothing
  expect_invisible(enableResume(appVersion = 2, maxSize = 1e6, maxNodeSize = 1e3, debounce = 0))
  expect_identical(
    getShinyOption("resume"),
    list(enabled = TRUE, appVersion = "2", maxSize = 1e6, maxNodeSize = 1e3, debounce = 0)
  )
  expect_invisible(disableResume())
  expect_identical(getShinyOption("resume"), c(list(enabled = FALSE), resume_defaults()))
})

test_that("enableResume() and disableResume() refuse to run inside a session", {
  withr::defer(shinyOptions(resume = NULL))
  s <- MockShinySession$new()
  expect_error(
    withReactiveDomain(s, enableResume()),
    "Call enableResume() at the top level of the app (in app.R before shinyApp(), or in global.R), not inside a session.",
    fixed = TRUE
  )
  expect_error(
    withReactiveDomain(s, disableResume()),
    "Call disableResume() at the top level of the app to turn resume off, or pass it a reactive.",
    fixed = TRUE
  )
  expect_null(getShinyOption("resume"))
})

test_that("enableResume() and disableResume() refuse to run from server.R", {
  # server.R is sourced only once the first session has started, after that
  # session resolved its settings, so the call would apply to later sessions only.
  withr::defer(shinyOptions(resume = NULL, appDir = NULL))
  for (fn in c("enableResume", "disableResume")) {
    dir <- withr::local_tempdir()
    writeLines("fluidPage()", file.path(dir, "ui.R"))
    writeLines(c(paste0(fn, "()"), "function(input, output) NULL"), file.path(dir, "server.R"))
    app <- shinyAppDir(dir)
    expect_error(
      app$serverFuncSource(),
      paste0("Call ", fn, "() in global.R, not server.R: server.R runs only once the first session has started."),
      fixed = TRUE
    )
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

test_that("disableResume(x) marks a node inside a session and rejects anything else", {
  s <- MockShinySession$new()
  withReactiveDomain(s, {
    v <- reactiveVal(1)
    out <- withVisible(disableResume(v))
    expect_false(out$visible)
    expect_identical(out$value, v)
  })
  expect_error(disableResume(42), "disableResume() expects a reactiveVal, reactiveValues, observer, or render function, not an object of class numeric.", fixed = TRUE)
})
