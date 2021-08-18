shinyAppFile(
  "wrapped.R",
  options = list(
    port = Sys.getenv(
      "SHINY_TEST_PORT_WRAPPED_2",
      stop("`Sys.env('SHINY_TEST_PORT_WRAPPED_2')` not found")
    )
  )
)
