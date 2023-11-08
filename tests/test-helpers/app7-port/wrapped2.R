shinyAppFile(
  "wrapped.R",
  options = list(
    port = as.numeric(Sys.getenv("SHINY_TESTTHAT_PORT_WRAPPED2", "8080"))
  )
)
