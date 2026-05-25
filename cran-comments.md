## Comments

#### 2025-12-08

Test has been removed from CRAN checks.

Also added a couple bug fixes as found by users.

Please let me know if you need any further changes.

Thank you,
Carson

#### 2025-12-04

Error:

```
Check Details
Version: 1.12.0
Check: tests
Result: ERROR
    Running ‘testthat.R’ [100s/394s]
  Running the tests in ‘tests/testthat.R’ failed.
  Complete output:
    > library(testthat)
    > library(shiny)
    >
    > test_check("shiny")
    Saving _problems/test-timer-35.R
    [ FAIL 1 | WARN 0 | SKIP 22 | PASS 1981 ]

    ══ Skipped tests (22) ══════════════════════════════════════════════════════════
    • File system is not case-sensitive (1): 'test-app.R:36:5'
    • I'm not sure of a great way to test this without timers. (1):
      'test-test-server.R:216:3'
    • Not testing in CI (1): 'test-devmode.R:17:3'
    • On CRAN (18): 'test-actionButton.R:59:1', 'test-busy-indication.R:1:1',
      'test-busy-indication.R:15:1', 'test-busy-indication.R:50:1',
      'test-otel-error.R:1:1', 'test-otel-mock.R:1:1', 'test-pkgdown.R:3:3',
      'test-reactivity.r:146:1', 'test-reactivity.r:1240:5',
      'test-reactivity.r:1240:5', 'test-stacks-deep.R:93:1',
      'test-stacks-deep.R:141:1', 'test-stacks.R:140:3', 'test-tabPanel.R:46:1',
      'test-tabPanel.R:66:1', 'test-tabPanel.R:73:1', 'test-tabPanel.R:83:1',
      'test-utils.R:177:3'
    • {shinytest2} is not installed (1): 'test-test-shinyAppTemplate.R:2:1'

    ══ Failed tests ════════════════════════════════════════════════════════════════
    ── Failure ('test-timer.R:35:3'): Unscheduling works ───────────────────────────
    Expected `timerCallbacks$.times` to be identical to `origTimes`.
    Differences:
    `attr(actual, 'row.names')` is an integer vector ()
    `attr(expected, 'row.names')` is a character vector ()


    [ FAIL 1 | WARN 0 | SKIP 22 | PASS 1981 ]
    Error:
    ! Test failures.
    Execution halted
```


#### 2025-12-03

```
Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_shiny.html>.

Please correct before 2025-12-17 to safely retain your package on CRAN.

The CRAN Team
```

## `R CMD check` results:

0 errors | 0 warning | 1 note

```
─  checking CRAN incoming feasibility ... [7s/70s] NOTE (1m 9.5s)
   Maintainer: ‘Carson Sievert <carson@posit.co>’

   Days since last update: 5
```


## revdepcheck results

We checked 1383 reverse dependencies (1376 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 31 packages

Issues with CRAN packages are summarised below.

### Failed to check

* AssumpSure
* boinet
* brms
* cheem
* ctsem
* detourr
* FAfA
* fio
* fitteR
* FossilSimShiny
* GDINA
* ggsem
* grandR
* hbsaems
* langevitour
* lavaan.shiny
* lcsm
* linkspotter
* loon.shiny
* MOsemiind
* MVN
* pandemonium
* polarisR
* RCTrep
* rstanarm
* semdrw
* shotGroups
* sphereML
* spinifex
* SurprisalAnalysis
* TestAnaAPP
