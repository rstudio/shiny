
#' Generate a Shiny package template
#' @export
shinyPackageSkeleton <- function(name, dir, twoFiles=FALSE, rDir=TRUE, examples=TRUE) {
  dir <- file.path(dir, name)
  if (file.exists(dir)){
    stop(dir, " already exists!")
  }
  dir.create(dir)

  # Little helper to resolve paths relative to our example
  examplePath <- function(path) {
    system.file(file.path("examples/12_counter/", path), package="shiny")
  }

  # The R/ dir
  rDir <- file.path(dir, "R")
  dir.create(rDir)
  if (examples){
    file.copy(examplePath("R/counter.R"), rDir)
  }


  # The tests/ dir
  testsDir <- file.path(dir, "tests")
  dir.create(testsDir)
  file.copy(examplePath("tests/shinytests.R"), file.path(testsDir, "shinytests.R"))
  file.copy(examplePath("tests/testthat.R"), file.path(testsDir, "testthat.R"))
  file.copy(examplePath("tests/integration.R"), file.path(testsDir, "integration.R"))
  if (!examples){
    dir.create(file.path(testsDir, "shinytests"))
    dir.create(file.path(testsDir, "testthat"))
    dir.create(file.path(testsDir, "integration"))
  } else {
    file.copy(examplePath("tests/shinytests"),
              testsDir, recursive=TRUE)
    file.copy(examplePath("tests/testthat"),
              testsDir, recursive=TRUE)
    file.copy(examplePath("tests/integration"),
              testsDir, recursive=TRUE)
  }


  # app/ui/server.R files
  if(!twoFiles){
    # Create app.R
    if (!examples) {
      file.create(file.path(dir, "app.R"))
    } else {
      uiLines <- readLines(examplePath("ui.R"))
      firstCodeLine <- which(grepl("^\\s*[^#]", uiLines, perl=TRUE))[1]
      uiLines[firstCodeLine] <- paste0("ui <- ", uiLines[firstCodeLine])
      ui <- paste(uiLines, collapse="\n") # TODO: does windows need \r?

      serverLines <- readLines(examplePath("server.R"))
      server <- paste(serverLines, collapse="\n") # TODO: does windows need \r?
      # Name the artifacts
      server <- sub("function", "server <- function", server)

      appR <- c(ui, "\n", server, "\n", "shinyApp(ui = ui, server = server)\n")
      writeLines(appR, file.path(dir, "app.R"))
    }
  } else {
    # Create server.R and ui.R
    if (!examples) {
      file.create(file.path(dir, "server.R"))
      file.create(file.path(dir, "ui.R"))
    } else {
      file.copy(examplePath("R/ui.R"), dir)
      file.copy(examplePath("R/server.R"), dir)
    }
  }
}

