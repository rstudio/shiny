## revdepcheck results

We checked 902 reverse dependencies (778 from CRAN + 124 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 2 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* bsplus
  checking tests ...

  * Notified on May 11, 2020
  * PR: https://github.com/ijlyttle/bsplus/pull/74
    * Merged: May 12, 2020


### Failed to check

* frailtypack
  * I can not figure out a way to install from source on macOS

* skeleSim
  * Error: object ‘strata<-’ is not exported by 'namespace:strataG'
  * False positive
