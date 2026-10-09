#' @include utils.R
NULL

# How long a record is kept. A store setting with no knob.
snapshot_ttl <- 86400

# Per-app caps: total bytes and record count in one app's store
# subdirectory, enforced by cachem with fifo eviction.
snapshot_store_max_size <- 1024^3
snapshot_store_max_n <- 1000L

# What enableResume() defaults to; also what an app gets when it made no
# call and a platform set SHINY_RESUME=TRUE.
resume_defaults <- function() {
  list(appVersion = NULL, maxSize = 50e6, maxNodeSize = 5e6, debounce = 1000, userDataExclude = character(0))
}

# The app's resume settings: its last enableResume() call as captured by
# shinyApp(), else SHINY_RESUME, else off. Sessions resolve it once, at start.
resume_settings <- function() {
  captured <- getShinyOption("resume")
  if (!is.null(captured)) return(captured)
  env <- Sys.getenv("SHINY_RESUME", unset = "")
  enabled <- nzchar(env) && isTRUE(as.logical(env))
  c(list(enabled = enabled), resume_defaults())
}
