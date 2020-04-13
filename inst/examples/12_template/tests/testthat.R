library(testthat)

# Run in the "current" environment, because shiny::runTests() is going to
# provision a new environment that's just for our test. And we'll want access to
# the supporting files that were already loaded into that env.
testthat::test_dir("./testthat", env = environment())
