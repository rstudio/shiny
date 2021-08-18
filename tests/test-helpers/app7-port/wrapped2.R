shinyAppFile(
  "wrapped.R",
  options = list(
    port = as.numeric(readLines(tempdir(), "shiny_testthat_port", "wrapped2"))
  )
)
