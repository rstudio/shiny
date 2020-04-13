
# The RStudio IDE offers a "Run Tests" button when it sees testthat tests but it runs
# in its own environment/process. Which means that any helpers we've loaded into our
# environment won't be visible. So we add this helper not because it's actually needed
# in the typical `shiny::runTests` workflow, but to make that IDE button work.
# Once the IDE adds proper support for this style, we'll be able to drop these files.
#
# Note that this may redundantly source the files in your R/ dir depending on your
# workflow.
library(shiny)
shiny::loadSupport("../../", renv = globalenv())
