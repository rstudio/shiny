## revdepcheck results

We checked 836 reverse dependencies (719 from CRAN + 117 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 3 packages

  * MissingDataGUI - RGtk2 issues
  * rgl - gcc / clang issues
  * RQuantLib - Needs rgl
