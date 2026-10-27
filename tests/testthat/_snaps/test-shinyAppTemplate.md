# shinyAppTemplate() creates the expected files for each combo

    Code
      for (combo in combos) {
        cat(paste0(combo, collapse = "_"), ":\n")
        cat(paste0("  ", template_files(combo), "\n"), sep = "")
      }
    Output
      all :
        R/example-module.R
        R/example.R
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-examplemodule.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R
        tests/testthat/test-sort.R
      app :
        app.R
      app_module :
        R/example-module.R
        app.R
      app_rdir :
        R/example.R
        app.R
      app_module_rdir :
        R/example-module.R
        R/example.R
        app.R
      app_tests :
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R
      app_module_tests :
        R/example-module.R
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-examplemodule.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R
      app_rdir_tests :
        R/example.R
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R
        tests/testthat/test-sort.R
      app_module_rdir_tests :
        R/example-module.R
        R/example.R
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-examplemodule.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R
        tests/testthat/test-sort.R
      app_shinytest_testthat :
        app.R
        tests/testthat.R
        tests/testthat/setup-shinytest2.R
        tests/testthat/test-server.R
        tests/testthat/test-shinytest2.R

