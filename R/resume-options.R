#' @include utils.R
NULL

# How long saved state is kept. A store setting with no knob.
snapshot_ttl <- 86400

# What the snapshot walk records. The sharing feature supplies a second
# instance.
snapshot_policy_reconnect <- function() {
  list(
    kinds = c("reactiveVal", "reactiveValues", "observer", "output"),
    userData = TRUE,
    keepInput = function(name, serializer) !identical(serializer, serializerUnserializable),
    files = "keep"
  )
}

# What enableResume() defaults to; also what an app gets when it made no
# call (a platform that sets SHINY_RESUME=TRUE gets these).
resume_defaults <- function() {
  list(appVersion = NULL, maxSize = 50e6, maxNodeSize = 5e6, debounce = 1000)
}

# The app's resume settings: its last enableResume() or
# disableResume() call as captured by shinyApp(), else SHINY_RESUME, else on
# locally and off under Shiny Server / Connect. Sessions resolve it once, at
# start.
resume_settings <- function() {
  captured <- getShinyOption("resume")
  if (!is.null(captured)) return(captured)
  env <- Sys.getenv("SHINY_RESUME", unset = "")
  enabled <- if (nzchar(env)) isTRUE(as.logical(env)) else !inShinyServer()
  c(list(enabled = enabled), resume_defaults())
}
