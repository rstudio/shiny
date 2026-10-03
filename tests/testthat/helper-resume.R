# Points the process store at a fresh temp cache dir for the calling test.
local_snapshot_store_dir <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  withr::local_envvar(R_USER_CACHE_DIR = dir, .local_envir = envir)
  snapshot_store_reset()
  withr::defer(snapshot_store_reset(), envir = envir)
  dir
}

# Calls enableResume(...) for the calling test, as an app.R would; sessions
# created afterwards resolve it.
local_enable_resume <- function(..., .env = parent.frame()) {
  old <- getShinyOption("resume")
  withr::defer(shinyOptions(resume = old), envir = .env)
  enableResume(...)
  invisible()
}

file_mode <- function(path) as.character(file.info(path)$mode)
