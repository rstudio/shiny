# omicsTools

<details>

* Version: 1.0.5
* GitHub: https://github.com/YaoxiangLi/omicsTools
* Source code: https://github.com/cran/omicsTools
* Date/Publication: 2023-07-03 16:20:02 UTC
* Number of recursive dependencies: 87

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

# PopED

<details>

* Version: 0.7.0
* GitHub: https://github.com/andrewhooker/PopED
* Source code: https://github.com/cran/PopED
* Date/Publication: 2024-10-07 19:30:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "PopED")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.4Mb
        test   1.1Mb
    ```

# shinyGovstyle

<details>

* Version: 0.1.0
* GitHub: https://github.com/moj-analytical-services/shinyGovstyle
* Source code: https://github.com/cran/shinyGovstyle
* Date/Publication: 2024-09-12 14:40:02 UTC
* Number of recursive dependencies: 48

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
* Number of recursive dependencies: 129

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
* Number of recursive dependencies: 134

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

