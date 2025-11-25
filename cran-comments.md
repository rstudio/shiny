## Comments

#### 2025-11-07

Hi CRAN,

We made changes to underlying structures that packages are not suppose to test. PRs were provided for each failing package.

Maintainer change: From Winston Chang to Carson Sievert.

Please let me know if you need any further information.

Thank you,
Carson

## `R CMD check` results:

The maintainer change is correctly detected. The URL check is flagging a 429 error from Wikipedia, which is likely temporary as the URL is valid when visited manually.

```
* checking CRAN incoming feasibility ... [19s] NOTE
Maintainer: 'Carson Sievert <barret@posit.co>'

New maintainer:
  Carson Sievert <barret@posit.co>
Old maintainer(s):
  Winston Chang <winston@posit.co>

Found the following (possibly) invalid URLs:
  URL: https://en.wikipedia.org/wiki/Reactive_programming
    From: README.md
    Status: 429
    Message: Too Many Requests
```


## Reverse dependency fixes

The revdep checks below are failing due to changes made in https://github.com/rstudio/shiny/pull/4249 .

Unresolved PRs submitted in 2025/06:
* omicsTools    - https://github.com/cheemalab/omicsTools/pull/1
* shinyGovstyle - https://github.com/dfe-analytical-services/shinyGovstyle/pull/155
* ShinyLink     - https://github.com/cdc-addm/ShinyLink/pull/3
* shinySbm      - https://github.com/Jo-Theo/shinySbm/pull/2


Unresolved PR submitted in 2025/10/29:
* biodosetools - PR made 2025/10/29 - https://github.com/biodosetools-team/biodosetools/pull/64
* inshiny      - PR made 2025/10/29 - https://github.com/nicholasdavies/inshiny/pull/1


## revdepcheck results

We checked 1386 reverse dependencies (1380 from CRAN + 6 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 6 new problems
 * We failed to check 17 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* biodosetools
  checking tests ... ERROR

* inshiny
  checking examples ... ERROR
  checking tests ... ERROR
  checking re-building of vignette outputs ... ERROR

* omicsTools
  checking tests ... ERROR

* shinyGovstyle
  checking tests ... ERROR

* ShinyLink
  checking tests ... ERROR

* shinySbm
  checking tests ... ERROR

### Failed to check

* cocktailApp
* ctsem
* FAfA
* fio
* FossilSimShiny
* GDINA
* ggsem
* grandR
* lavaan.shiny
* lcsm
* loon.shiny
* rstanarm
* semdrw
* sphereML
* SurprisalAnalysis
* TestAnaAPP
* tricolore
