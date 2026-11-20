## Comments

This release fixes the test errors on the r-devel check flavors in
<https://cran.r-project.org/web/checks/check_results_shiny.html>. The tests in
`test-reactlog.R` expected the old `str()` output; they now accept the output
of both R-release and R-devel.

It also includes bug fixes reported by users and a new bundled agent skill.

Please let me know if you need any further changes.

Thank you,
Carson

## `R CMD check` results:

0 errors | 0 warnings | 1 note

```
❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Carson Sievert <carson@posit.co>’

  Found the following (possibly) invalid URLs:
    URL: https://ror.org/03wc8by49
      From: man/shiny-package.Rd
      Status: 429
      Message: Too Many Requests

  Found the following (possibly) invalid ROR IDs:
    ID: 03wc8by49	(from: DESCRIPTION)
```

The ROR URL and ID are valid; ror.org rate-limited the check with a 429.

## revdepcheck results

We checked 1723 reverse dependencies (1719 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 4 packages

Issues with CRAN packages are summarised below.

### Failed to check

* ctsem
* epts
* loon.shiny
* viewpoly
