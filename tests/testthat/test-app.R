test_that("files are loaded into the right env", {
  renv <- new.env(parent=environment())
  genv <- new.env(parent=environment())

  loadSupport(test_path("../test-helpers/app1-standard"), renv=renv, globalrenv=genv)
  expect_equal(get("helper1", renv, inherits=FALSE), 123)
  expect_equal(get("helper2", renv, inherits=FALSE), "abc")

  expect_equal(get("global", genv, inherits=FALSE), "ABC")
})

test_that("Can suppress sourcing global.R", {
  # Confirm that things blow up if we source global.R
  expect_error(loadSupport(test_path("../test-helpers/app3-badglobal")))

  # Shouldn't see an error now that we're suppressing global sourcing.
  renv <- loadSupport(test_path("../test-helpers/app3-badglobal"), globalrenv=NULL)

  # But other helpers are still sourced
  expect_true(exists("helper1", envir=renv))
})

test_that("nested helpers are not loaded", {
  loadSupport(test_path("../test-helpers/app2-nested"), renv=environment(), globalrenv=NULL)
  expect_equal(helper1, 456)
  expect_false(exists("helper2"))
})

test_that("app with both r/ and R/ prefers R/", {
  ## App 4 already has a lower-case r/ directory. Try to create an upper.
  dir <- test_path("../test-helpers/app4-both/R")
  tryCatch({
    dir.create(dir)
    teardown(unlink(dir, recursive = TRUE))
  }, warning = function(w) {
    testthat::skip("File system is not case-sensitive")
  })
  writeLines("upperHelper <- 'abc'", file.path(dir, "upper.R"))

  renv <- loadSupport(test_path("../test-helpers/app4-both"))

  expect_false(exists("lowerHelper", envir=renv))
  expect_equal(get("upperHelper", envir=renv), "abc")
})

test_that("With ui/server.R, global.R is loaded before R/ helpers and into the right envs", {
  calls <- list()
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    NULL
  }

  # Temporarily opt-in to R/ file autoloading
  withr::local_options(list(shiny.autoload.r=TRUE))

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadSupport
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadSupport, sourceUTF8 = sourceStub)
  sad <- rewire(shinyAppDir_serverR, sourceUTF8 = sourceStub, loadSupport = loadSpy)

  sa <- sad(normalizePath(test_path("../test-helpers/app1-standard")))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen three calls -- first to global then to the helpers
  expect_length(calls, 3)
  expect_match(calls[[1]][[1]], "global\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "helperCap\\.R$", perl=TRUE)
  expect_match(calls[[3]][[1]], "helperLower\\.r$", perl=TRUE)

  # Check environments
  # global.R loaded into the global env
  gEnv <- calls[[1]]$envir
  expect_identical(gEnv, globalenv())

  # helpers are loaded into a child of the global env
  helperEnv1 <- calls[[2]]$envir
  helperEnv2 <- calls[[3]]$envir
  expect_identical(helperEnv1, helperEnv2)
  expect_identical(parent.env(helperEnv1), globalenv())

  calls <- NULL
  # Source the server
  sa$serverFuncSource()
  expect_length(calls, 1)
  # server.R is sourced into a child environment of the helpers
  expect_match(calls[[1]][[1]], "/server\\.R$")
  expect_identical(parent.env(calls[[1]]$envir), helperEnv1)

  calls <- NULL
  # Invoke the UI by simulating a request
  sa$httpHandler(list())
  expect_length(calls, 1)
  # ui.R is sourced into a child environment of the helpers
  expect_match(calls[[1]][[1]], "ui\\.R$")
  expect_identical(parent.env(calls[[1]]$envir), helperEnv1)
})


test_that("Loading supporting R files is opt-out", {
  calls <- list()
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    NULL
  }

  # Temporarily unset autoloading option
  withr::local_options(list(shiny.autoload.r = NULL))

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadSupport
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadSupport, sourceUTF8 = sourceStub)
  sad <- rewire(shinyAppDir_serverR, sourceUTF8 = sourceStub, loadSupport = loadSpy)

  sa <- sad(normalizePath(test_path("../test-helpers/app1-standard")))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen three calls from global.R -- helpers are enabled
  expect_length(calls, 3)
  expect_match(calls[[1]][[1]], "global\\.R$", perl=TRUE)
})


test_that("Disabling supporting R files works", {
  calls <- list()
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    NULL
  }

  # Temporarily unset autoloading option
  withr::local_options(list(shiny.autoload.r = FALSE))

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadSupport
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadSupport, sourceUTF8 = sourceStub)
  sad <- rewire(shinyAppDir_serverR, sourceUTF8 = sourceStub, loadSupport = loadSpy)

  sa <- sad(normalizePath(test_path("../test-helpers/app1-standard")))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen one calls from global.R -- helpers are disabled
  expect_length(calls, 1)
  expect_match(calls[[1]][[1]], "global\\.R$", perl=TRUE)
})

test_that("app.R is loaded after R/ helpers and into the right envs", {
  calls <- list()
  sourceSpy <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    do.call(sourceUTF8, list(...))
  }

  # Temporarily opt-in to R/ file autoloading
  withr::local_options(list(shiny.autoload.r = TRUE))

  # + shinyAppDir_serverR
  # +--- sourceUTF8
  # +--+ loadSupport
  # |  +--- sourceUTF8
  loadSpy <- rewire(loadSupport, sourceUTF8 = sourceSpy)
  sad <- rewire(shinyAppDir_appR, sourceUTF8 = sourceSpy, loadSupport = loadSpy)

  sa <- sad("app.R", normalizePath(test_path("../test-helpers/app2-nested")))
  sa$onStart()
  sa$onStop() # Close down to free up resources

  # Should have seen three calls -- first to two helpers then to app.R
  expect_length(calls, 2)
  expect_match(calls[[1]][[1]], "helper\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "app\\.R$", perl=TRUE)

  # Check environments
  # helpers are loaded into a child of the global env
  helperEnv1 <- calls[[1]]$envir
  expect_identical(parent.env(helperEnv1), globalenv())

  # app.R is sourced into a child environment of the helpers
  expect_identical(parent.env(calls[[2]]$envir), helperEnv1)
})

test_that("global.R and sources in R/ are sourced in the app directory", {
  appDir <- test_path("../test-helpers/app1-standard")
  appGlobalEnv <- new.env(parent = globalenv())
  appEnv <- new.env(parent = appGlobalEnv)
  loadSupport(appDir, renv = appEnv, globalrenv = appGlobalEnv)

  # Set by ../test-helpers/app1-standard/global.R
  expect_equal(normalizePath(appGlobalEnv$global_wd), normalizePath(appDir))

  # Set by ../test-helpers/app1-standard/R/helperCap.R
  expect_equal(normalizePath(appEnv$source_wd), normalizePath(appDir))
})

test_that("Setting options in various places works", {
  withr::local_options(list(shiny.launch.browser = FALSE))

  # Use random ports to avoid errors while running revdepcheck in parallel
  # https://github.com/rstudio/shiny/pull/3488
  # Try up to 100 times to find a unique port
  for (i in 1:100) {
    test_app_port      <- httpuv::randomPort()
    test_wrapped2_port <- httpuv::randomPort()
    test_option_port   <- httpuv::randomPort()
    # If all ports are unique, move on
    if (length(unique(
      c(test_app_port, test_wrapped2_port, test_option_port)
    )) == 3) {
      break
    }
  }
  # Use system envvars to pass values into the tests
  withr::local_envvar(
    list(
      SHINY_TESTTHAT_PORT_APP      = as.character(test_app_port),
      SHINY_TESTTHAT_PORT_WRAPPED2 = as.character(test_wrapped2_port),
      SHINY_TESTTHAT_PORT_OPTION   = as.character(test_option_port)
    )
  )

  appDir <- test_path("../test-helpers/app7-port")
  withPort <- function(port, expr) {
    withr::local_options(list(app7.port = port))

    force(expr)
  }

  expect_port <- function(expr, port) {
    later::later(~stopApp(), 0)
    testthat::expect_message(expr, paste0("Listening on http://127.0.0.1:", port), fixed = TRUE)
  }

  expect_port(runApp(appDir), test_app_port)

  appObj <- source(file.path(appDir, "app.R"))$value
  expect_port(print(appObj), test_app_port)

  appObj <- shinyAppDir(appDir)
  expect_port(print(appObj), test_app_port)

  # The outermost call (shinyAppDir) has its options take precedence over the
  # options in the inner call (shinyApp in app7-port/app.R).
  options_port <- httpuv::randomPort()
  appObj <- shinyAppDir(appDir, options = list(port = options_port))
  expect_port(print(appObj), options_port)
  expect_port(runApp(appObj), options_port)

  # Options set directly on the runApp call take precedence over everything.
  provided_port <- httpuv::randomPort()
  expect_port(runApp(appObj, port = provided_port), provided_port)

  # wrapped.R calls shinyAppDir("app.R")
  expect_port(runApp(file.path(appDir, "wrapped.R")), test_app_port)
  # wrapped2.R calls shinyAppFile("wrapped.R", options = list(port = 3032))
  expect_port(runApp(file.path(appDir, "wrapped2.R")), test_wrapped2_port)

  shiny_port_orig <- getOption("shiny.port")
  # Calls to options(shiny.port = xxx) within app.R should also work reliably
  expect_port(runApp(file.path(appDir, "option.R")), test_option_port)
  # Ensure that option was unset/restored
  expect_identical(getOption("shiny.port"), shiny_port_orig)
  # options(shiny.port = xxx) is overrideable
  override_port <- httpuv::randomPort()
  appObj <- shinyAppFile(file.path(appDir, "option.R"), options = list(port = override_port))
  expect_port(print(appObj), override_port)

  # onStop still works even if app.R has an error (ensure option was unset)
  expect_error(runApp(file.path(appDir, "option-broken.R")), "^boom$")
  expect_null(getOption("shiny.port"))


})
