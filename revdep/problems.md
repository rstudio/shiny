# biodosetools

<details>

* Version: 3.7.1
* GitHub: https://github.com/biodosetools-team/biodosetools
* Source code: https://github.com/cran/biodosetools
* Date/Publication: 2025-10-22 08:10:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "biodosetools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biodosetools)
      Loading required package: shiny
      Loading required package: golem
      > 
      > test_check("biodosetools")
      ! Problem with `glm()` -> constraint ML optimization will be used instead
    ...
      
      lines(actual) vs lines(expected)
      - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">"
      - "  <span class=\"action-label\">go</span>"
      - "</button>"
      + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>"
      
      [ FAIL 2 | WARN 1 | SKIP 1 | PASS 455 ]
      Error: Test failures
      Execution halted
    ```

# inshiny

<details>

* Version: 0.1.0
* GitHub: https://github.com/nicholasdavies/inshiny
* Source code: https://github.com/cran/inshiny
* Date/Publication: 2025-09-09 14:00:13 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "inshiny")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘inshiny-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: inline_button
    > ### Title: Inline action button
    > ### Aliases: inline_button
    > 
    > ### ** Examples
    > 
    > ui <- bslib::page_fixed(
    ...
    +             label = shiny::span(style = "font-style:italic", "button"),
    +             icon = shiny::icon("play"),
    +             meaning = "Update button", accent = "success"),
    +         "."
    +     )
    + )
    Error in check_tags(widget, shiny::tags$button(), "shiny::actionButton()") : 
      Unexpected tag structure from shiny::actionButton(). Please contact the package maintainer.
    Calls: <Anonymous> ... div -> dots_list -> inline -> inline_button -> check_tags
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ── Error ('test-action.R:8:5'): button is stable ───────────────────────────────
      Error in `check_tags(widget, shiny::tags$button(), "shiny::actionButton()")`: Unexpected tag structure from shiny::actionButton(). Please contact the package maintainer.
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(...) at test-action.R:8:5
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 2 | WARN 0 | SKIP 9 | PASS 24 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘inshiny.Rmd’ using rmarkdown
    
    Quitting from inshiny.Rmd:64-86 [unnamed-chunk-3]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `check_tags()`:
    ! Unexpected tag structure from shiny::actionButton(). Please contact the package maintainer.
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'inshiny.Rmd' failed with diagnostics:
    Unexpected tag structure from shiny::actionButton(). Please contact the package maintainer.
    --- failed re-building ‘inshiny.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘inshiny.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# omicsTools

<details>

* Version: 1.0.5
* GitHub: https://github.com/YaoxiangLi/omicsTools
* Source code: https://github.com/cran/omicsTools
* Date/Publication: 2023-07-03 16:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "omicsTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
      
      lines(actual) vs lines(expected)
      - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">"
      - "  <span class=\"action-label\">go</span>"
      - "</button>"
      + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>"
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 94 ]
      Error: Test failures
      Execution halted
    ```

# shinyGovstyle

<details>

* Version: 0.1.0
* GitHub: https://github.com/moj-analytical-services/shinyGovstyle
* Source code: https://github.com/cran/shinyGovstyle
* Date/Publication: 2024-09-12 14:40:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "shinyGovstyle")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(shinyGovstyle)
      > 
      > test_check("shinyGovstyle")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 125 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-backlink_Input.R:4:3'): backlink works ───────────────────────
      backlink_check$children[[1]][[2]] not identical to "Back".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 125 ]
      Error: Test failures
      Execution halted
    ```

# ShinyLink

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/ShinyLink
* Date/Publication: 2023-01-18 11:40:05 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "ShinyLink")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
      
      lines(actual) vs lines(expected)
      - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">"
      - "  <span class=\"action-label\">go</span>"
      - "</button>"
      + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>"
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 145 ]
      Error: Test failures
      Execution halted
    ```

# shinySbm

<details>

* Version: 0.1.5
* GitHub: https://github.com/Jo-Theo/shinySbm
* Source code: https://github.com/cran/shinySbm
* Date/Publication: 2023-09-07 21:50:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "shinySbm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
    ...
      
      lines(actual) vs lines(expected)
      - "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">"
      - "  <span class=\"action-label\">go</span>"
      - "</button>"
      + "<button id=\"go_filter\" type=\"button\" class=\"btn btn-default action-button\" style=\"display: none;\">go</button>"
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 141 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) FungusTreeNetwork.Rd:15-21: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) FungusTreeNetwork.Rd:22-28: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) FungusTreeNetwork.Rd:33-34: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) FungusTreeNetwork.Rd:33: Lost braces; missing escapes or markup?
        33 |      \item{tree_tree}{Results of \code{estimateSimpleSBM} for {sbm}
           |                                                               ^
    checkRd: (-1) FungusTreeNetwork.Rd:35-36: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) FungusTreeNetwork.Rd:35: Lost braces; missing escapes or markup?
        35 |      \item{fungus_tree}{Results of \code{estimateBipartiteSBM} for {sbm}
           |                                                                    ^
    ...
    checkRd: (-1) visSbm.default.Rd:25: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:26: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:43-44: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:45: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:46: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:47: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:48: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:49: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:50: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) visSbm.default.Rd:51: Lost braces in \itemize; meant \describe ?
    ```

