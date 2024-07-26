# EgoCor

<details>

* Version: 1.2.0
* GitHub: https://github.com/julia-dyck/EgoCor
* Source code: https://github.com/cran/EgoCor
* Date/Publication: 2024-03-28 18:10:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "EgoCor")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Intro_to_EgoCor.Rmd’
      ...
    Warning in gstat::fit.variogram(emp.sv, model = v, fit.sills = TRUE, fit.ranges = TRUE,  :
      linear model has singular covariance matrix
    Warning in gstat::fit.variogram(emp.sv, model = v, fit.sills = TRUE, fit.ranges = TRUE,  :
      linear model has singular covariance matrix
    Warning in gstat::fit.variogram(emp.sv, model = v, fit.sills = TRUE, fit.ranges = TRUE,  :
      No convergence after 200 iterations: try different initial values?
    
      When sourcing ‘Intro_to_EgoCor.R’:
    Error: missing value where TRUE/FALSE needed
    Execution halted
    
      ‘Intro_to_EgoCor.Rmd’ using ‘UTF-8’... failed
    ```

## Newly fixed

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Intro_to_EgoCor.Rmd’ using rmarkdown
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘EgoCor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vario.reg.prep
    > ### Title: Adjustment for covariates before semi-variogram model fitting
    > ### Aliases: vario.reg.prep
    > 
    > ### ** Examples
    > 
    > ## Example 1
    ...
    + }
    Warning in check_dep_version() :
      ABI version mismatch: 
    lme4 was built with Matrix ABI version 1
    Current Matrix ABI version is 0
    Please re-install lme4 from source or restore original ‘Matrix’ package
    Error in initializePtr() : 
      function 'cholmod_factor_ldetA' not provided by package 'Matrix'
    Calls: <Anonymous> ... initialize -> <Anonymous> -> initializePtr -> .Call
    Execution halted
    ```

# mxfda

<details>

* Version: 0.2.1
* GitHub: https://github.com/julia-wrobel/mxfda
* Source code: https://github.com/cran/mxfda
* Date/Publication: 2024-05-08 11:00:02 UTC
* Number of recursive dependencies: 220

Run `revdepcheck::cloud_details(, "mxfda")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
    ```

