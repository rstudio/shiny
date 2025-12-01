## Comments

#### 2025-12-01

Hi CRAN,

We made changes to underlying structures that packages are not suppose to test. PRs were provided for each failing package.

Maintainer change: From Winston Chang to Carson Sievert.

Please let me know if you need any further information.

Thank you,
Carson

## `R CMD check` results:

The maintainer change is correctly detected. The URL check sometimes flags a 429
error from Wikipedia, which is a temporary issue since the URL is valid when
visited manually.

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

## Reverse dependency false positives

* SouthParkRshiny - New NOTE about installed package size. This is unrelated to any new changes in Shiny.

> ```
> * checking installed package size ... NOTE
>   installed size is  8.6Mb
>   sub-directories of 1Mb or more:
>     data   8.0Mb
> ```

## revdepcheck results

We checked 1395 reverse dependencies (1388 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 7 new problems
 * We failed to check 21 packages

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

* SouthParkRshiny
  checking installed package size ... NOTE

### Failed to check

* boinet
* ctsem
* FAfA
* fio
* FossilSimShiny
* GDINA
* ggsem
* grandR
* hbsaems
* lavaan.shiny
* lcsm
* linkspotter
* loon.shiny
* MOsemiind
* MVN
* RCTrep
* rstanarm
* semdrw
* sphereML
* SurprisalAnalysis
* TestAnaAPP
